
# libraries ---------------------------------------------------------------

library(tidyverse)
library(rio)
library(janitor)
library(MSstats)
library(UniprotR)


# data import -------------------------------------------------------------

psm <- import("data/Adams_Hira_IPMS_PSMs.txt")
summary_file <- import("data/summary.txt")
raw <- import("data/rawDiag_data.csv")

# cleaning ----------------------------------------------------------------


# Annotation data
annot <- summary_file |> 
  clean_names() |> 
  filter(raw_file != "Total") |> 
  select(raw_file, experiment) |> 
  rename(Run = raw_file,
         Condition = experiment) |> 
  mutate(Condition = str_remove_all(Condition, "_N[1-9]+")) |> 
  arrange(Condition) %>%
  mutate(BioReplicate = 1:nrow(.))

# rawDiag output
raw <- raw |> 
  clean_names() |> 
  mutate(filename = str_remove_all(filename, ".raw")) |> 
  rename(Run = filename) %>%
  full_join(annot, .)

# formatting names to match MSstats requirements
names(psm) <- gsub(" ", ".", names(psm))

# Removing .raw extension from file names
psm$Spectrum.File <- gsub(".raw", "", psm$Spectrum.File)


# TIC normalization -------------------------------------------------------


# TIC medians for normalization
group_median <- tapply(raw$tic[raw$ms_order == "Ms"], raw$Run[raw$ms_order == "Ms"], function(x) median(log2(x)))
global_median <- median(log2(raw$tic[raw$ms_order == "Ms"]), na.rm = TRUE)

# normalizing by TIC
psm <- psm |> 
  filter(Contaminant == FALSE) |> 
  mutate(Run = Spectrum.File,
         Precursor.Area.Norm = log2(`Precursor.Abundance`) - group_median[Run] + global_median,
         Precursor.Area.Norm = 2^Precursor.Area.Norm)

rm(global_median, group_median);gc()


# Fetching protein information from Uniprot -------------------------------


# Extracting protein accession to use as input for UniprotR functions
prot_names <- psm |> 
  select(Master.Protein.Accessions) |> 
  distinct() |> 
  rename(Protein = Master.Protein.Accessions) %>% 
  mutate(index = 1:nrow(.),
         Protein = str_remove_all(Protein, " "),
         Protein = strsplit(Protein, split = ";")) %>% 
  unnest(Protein)


###########################################################################
#### This takes a while. Run this line and then go and grab a coffee ######

# Connecting to Uniprot to fetch protein information
system.time(
  uniprot_proteins <- GetNamesTaxa(prot_names$Protein)
)

########################## How was the coffee? ############################
###########################################################################

# cleaning dataframe
uniprot_proteins <- uniprot_proteins %>% 
  rownames_to_column("Protein")

# Joining protein names with the Uniprot information
prot_names <- left_join(prot_names, uniprot_proteins) %>% 
  clean_names() 

# cleaning up primary gene name 
prot_names$gene <- unlist(lapply(prot_names$gene_names_primary, function(x){
  unlist(strsplit(x, split = ";"))[1]
}))

# formatting dataframe to match protein grouping
prot_names <- prot_names %>% 
  select(protein, entry_name, protein_names, gene, index) %>%
  group_by(index) %>%
  summarize(protein = paste(protein, collapse = "; "),
            entry_name = paste(entry_name, collapse = "; "),
            protein_names = paste(protein_names, collapse = "; "),
            gene = paste(gene, collapse = "; ")) %>%
  select(-index) 


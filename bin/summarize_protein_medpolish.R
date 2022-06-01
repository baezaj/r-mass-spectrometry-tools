
require(tidyverse)

# Protein summarization ---------------------------------------------------


###################
#### Functions ####
###################

# Summarizing Protein abundance
protein_summary_medpolish <- function(data){
  
  # selecting the data
  tmp <- data %>%
    select(group, sample_id, run_order, PeptideSeq, PeptideModSeq, PrecursorCharge, abundance_norm) %>%
    unite(id, group, sample_id, run_order, sep = "_") %>%
    unite(feature, PeptideSeq, PeptideModSeq, PrecursorCharge, sep = "_") %>%
    mutate(abundance_norm = 2^abundance_norm) %>%
    spread(feature, abundance_norm) %>%
    column_to_rownames("id")
  
  # Tukey Median polish
  meddata  <-  medpolish(tmp,
                         na.rm=TRUE,
                         trace.iter = FALSE)
  
  # Extracting the results
  tmpresult <- data.frame(abundance = (meddata$overall + meddata$row)) %>%
    rownames_to_column("id") %>%
    mutate(abundance = log2(abundance))
  
}

# Counting the number of peptides per protein
count_peptide_per_group <- function(x){
  length(unique(x$PeptideSeq))
}

###################
## End Functions ##
###################


proteinquants <- quant %>%
  filter(cv < 50) %>%
  group_by(ProteinGroup, ProteinAccession, ProteinDescription, GeneName, organism) %>%
  nest() %>%
  mutate(peptide_n = map_dbl(data, count_peptide_per_group)) %>%
  mutate(abundance = map(data, protein_summary_medpolish)) %>%
  select(-data) %>%
  unnest(abundance)


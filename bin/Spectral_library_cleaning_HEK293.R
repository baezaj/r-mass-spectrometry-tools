options(stringsAsFactors = FALSE)
library(dplyr)
library(tidyr)
library(stringr)

###########################################################################
spec_lib <- read.csv("Hek_stoich_curve_12C_D3_Acetyl.csv")
spec_lib <- spec_lib %>% select(1:14, -5, -6, -7, 20, 22, 24)
spec_lib <- spec_lib %>% arrange(StrippedPeptide, FragmentMz)
spec_lib$k_count <- str_count(spec_lib$StrippedPeptide, pattern = "K")
spec_lib$acetyl_count <- str_count(spec_lib$ModifiedPeptide, pattern = "Acetyl")
spec_lib$StrippedPeptide <- as.character(spec_lib$StrippedPeptide)

spec_lib$fragment_ion_sequence <- NA

ptm <- proc.time()

for(i in seq_along(spec_lib$fragment_ion_sequence)){
  sequence.vector <- strsplit(spec_lib$StrippedPeptide[i], split = "")[[1]]
  vector.length   <- length(sequence.vector)
  
  # Generates the y-ion sequence  
  if(spec_lib$FragmentType[i] == "y"){
    spec_lib$fragment_ion_sequence[i] <- 
      paste(sequence.vector[((length(sequence.vector) - spec_lib$FragmentNumber[i]) + 1):
                              length(sequence.vector)], collapse = "")
  }
  
  # Generates the b-ion sequence  
  if(spec_lib$FragmentType[i] == "b"){
    spec_lib$fragment_ion_sequence[i] <- 
      paste(sequence.vector[1:(spec_lib$FragmentNumber[i])], collapse = "")
  }
  
}

proc.time() - ptm

spec_lib$frag_k_count <- str_count(spec_lib$fragment_ion_sequence, pattern = "K")


###########################################################################
# generating a unique id for each fragment ion species
spec_lib_unique <- spec_lib %>% select(-5, -6, -11) %>% distinct()
spec_lib_unique$id <- 1:nrow(spec_lib_unique)

# Merging the peptide fragment id with the spectral library
spec_lib <- merge(spec_lib, spec_lib_unique)
spec_lib <- spec_lib %>% arrange(id)
spec_lib$ack_12 <- NA
spec_lib$ack_13 <- NA
spec_lib$ack_12 <- str_count(spec_lib$LabeledPeptide, "\\[Acetyl \\(K")
spec_lib$ack_13 <- str_count(spec_lib$LabeledPeptide, "\\[Acetyl D3 \\(K")

# setting up a dataframe 
names(spec_lib)[]
spec_lib_isotope <- data.frame("PrecursorCharge" = numeric(0), 
                               "IntModifiedPeptide" = character(0),
                               "ModifiedPeptide" = character(0),
                               "StrippedPeptide" = character(0),
                               "FragmentLossType" = character(0), 
                               "FragmentNumber" = numeric(0),
                               "FragmentType" = character(0),
                               "FragmentCharge" = numeric(0),
                               "k_count" = numeric(0), 
                               "fragment_ion_sequence" = character(0),
                               "frag_k_count" = numeric(0),
                               "LabeledPeptide" = character(0),
                               "PrecursorMz" = numeric(0),
                               "FragmentMz" = numeric(0),
                               "id" = numeric(0),
                               "ack_12" = numeric(0), 
                               "ack_13" = numeric(0),
                               "isotope_label" = character(0))

spec_lib$isotope_label <- NA

ptm <- proc.time()
for(i in seq_len(length(spec_lib_unique$id))){
  # creating a temporary dataframe
  temp_df <- spec_lib[which(spec_lib$id == i), ]
  temp_df <- temp_df %>% arrange(PrecursorMz, FragmentMz)
  
  # No lysines
  if(length(temp_df$id) == 1){
    temp_df$isotope_label[1] <- "L"
  }
  # 1 lysine
  if(length(temp_df$id) == 2){
    temp_df$isotope_label[1] <- "L"
    temp_df$isotope_label[2] <- "H"
  }
  # 2 lysines
  if(length(temp_df$id) == 4){
    temp_df$isotope_label[1] <- "LL"
    temp_df$isotope_label[2] <- "LH"
    temp_df$isotope_label[3] <- "LH"
    temp_df$isotope_label[4] <- "HH"
  }
  # 3 lysines
  if(length(temp_df$id) == 8){
    temp_df$isotope_label[1] <- "LLL"
    temp_df$isotope_label[2] <- "LLH"
    temp_df$isotope_label[3] <- "LLH"
    temp_df$isotope_label[4] <- "LLH"
    temp_df$isotope_label[5] <- "LHH"
    temp_df$isotope_label[6] <- "LHH"
    temp_df$isotope_label[7] <- "LHH"
    temp_df$isotope_label[8] <- "HHH"
  }
  spec_lib_isotope <- rbind(temp_df, spec_lib_isotope)
}
proc.time() - ptm
# time to complete task on MacBookPro is ___ sec
# time to complete task on PC is 740 sec!!!!!!!!

spec_lib_isotope <- spec_lib_isotope %>% rename(FG.Charge = PrecursorCharge, 
                                                EG.ModifiedPeptide = IntModifiedPeptide,
                                                EG.StrippedSequence = StrippedPeptide,
                                                F.FrgLossType = FragmentLossType, 
                                                F.FrgNum = FragmentNumber,
                                                F.FrgType = FragmentType,
                                                F.FrgZ = FragmentCharge,
                                                FG.PrecMz = PrecursorMz,
                                                F.FrgMz = FragmentMz)

#write.csv(spec_lib_isotope, file = "HEK_stoich_curve_spec_lib_IsotopeLabel.csv")

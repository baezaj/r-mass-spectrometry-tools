

# Convert Spectronaut Peptide to EncyclopeDIA format ----------------------

convert_spectronaut_to_encyclopedia_format <- function(data){
  
  
  # libraries ---------------------------------------------------------------
  
  require(stringr)
  
  
  # error handling ----------------------------------------------------------
  
  required_column_names <- c("R.FileName",
                             "PG.ProteinAccessions",
                             "PG.ProteinDescriptions",
                             "EG.PrecursorId",
                             "EG.Qvalue",
                             "EG.TotalQuantity (Settings)")  
  
  if(!all(required_column_names %in% names(data))){
    stop(paste0("Input does not contain the required columns. Go back and check!"))
  }
  
  
  # Formatting table --------------------------------------------------------
  
  # Filtering for precursor q.value < 0.01
  spectronaut_data <- data[data$EG.Qvalue < 0.01,]
  
  # Selecting only required columns
  spectronaut_data <- spectronaut_data[required_column_names]
  
  # Renaming columns to match EncyclopeDIA output
  colnames(spectronaut_data) <- c("SourceFile",
                                  "ProteinAccession",
                                  "ProteinDescription",
                                  "EG.PrecursorId",
                                  "EG.Qvalue",
                                  "TotalIntensity")
  
  # Adding two columns not present in Spectronaut. Default values are NA
  spectronaut_data$GeneName <- NA
  spectronaut_data$ProteinGroup <- NA
  
  # Cleaning up the Precursor ID column
  spectronaut_data$EG.PrecursorId <- gsub("_", "", spectronaut_data$EG.PrecursorId)
  
  # Splitting the EG.PrecursorId column into...
  spectronaut_data[c("PeptideModSeq", "PrecursorCharge")] <- str_split_fixed(spectronaut_data$EG.PrecursorId, pattern = "\\.", n = 2)
  
  # Cleaning up the PeptideModSeq column
  spectronaut_data$PeptideModSeq <- gsub(" \\s*\\([^\\)]+\\)", "", spectronaut_data$PeptideModSeq)
  
  # Extracting the stripped peptide sequence
  spectronaut_data$PeptideSeq <- gsub("\\[.*?\\]", "", spectronaut_data$PeptideModSeq)
  
  # Reordering columns
  spectronaut_data <- spectronaut_data[,c("SourceFile", "ProteinGroup", "ProteinAccession", "ProteinDescription", "GeneName", 
                                          "PeptideSeq", "PeptideModSeq", "PrecursorCharge", "TotalIntensity")]
  
  # final data table
  return(spectronaut_data)
  
  
  
}


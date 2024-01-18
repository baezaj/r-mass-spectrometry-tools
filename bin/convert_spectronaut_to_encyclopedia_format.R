
convert_spectronaut_to_encyclopedia_format <- function(data){
  
  
  require(stringr)
  
  
  # Error Handling ----------------------------------------------------------
  
  required_column_names <- c("R.FileName", 
                             "R.TotalIonCurrent (MS1)",
                             "PG.Genes",
                             "PG.FASTAHeader",
                             "PG.ProteinDescriptions", 
                             "EG.PrecursorId", 
                             "EG.Qvalue", 
                             "EG.ApexRT",
                             "EG.TotalQuantity (Settings)",
                             "FG.TheoreticalMz")
  
  if(!all(required_column_names %in% names(data))){
    
    stop(paste0("Input data does not contain the required columns. Go back and get ", 
                paste(required_column_names[!required_column_names %in% names(data)], collapse = ", "), 
                " from Spectronaut!!!"))
    
  }
  
  
  # Formatting table --------------------------------------------------------
  
  # Filtering for precursor q.value < 0.01
  spectronaut_data <- data[data$EG.Qvalue < 0.01,]
  
  # Selecting only required columns
  spectronaut_data <- spectronaut_data[required_column_names]
  
  # Changing column names to match EncyclopeDIA output
  colnames(spectronaut_data) <- c("Filename",
                                  "TIC",
                                  "GeneName",
                                  "FastaHeader",
                                  "ProteinDescription",
                                  "EG.PrecursorId",
                                  "EG.Qvalue",
                                  "RTInMin",
                                  "TotalIntensity",
                                  "PrecursorMz")
  
  
  # Creating and cleaning ProteinAccession column ---------------------------
  
  
  # Formatting the Fasta Header field to include multiple proteins in the protein group
  proteins <- spectronaut_data %>%
    select(FastaHeader) %>% 
    distinct() %>% 
    mutate(accession = strsplit(FastaHeader, split = ";")) %>% 
    unnest(accession)
  
  # Formatting the temporary accession column
  # This is to match the EncyclopeDIA output
  proteins$accession <- gsub("^>", "", proteins$accession)
  proteins$accession <- substr(proteins$accession, 
                               start = 1, 
                               stop = regexpr(" ", proteins$accession) - 1)
  
  # Collapsing the protein groups into a single entry
  proteins <- proteins %>% 
    group_by(FastaHeader) %>% 
    summarize(ProteinAccession = paste(accession, collapse = ";")) %>% 
    ungroup()
  
  # Joining the two dataframes
  # This command adds the ProteinAccession to the dataframe
  spectronaut_data <- full_join(spectronaut_data, proteins)
  
  
  # Formatting peptide sequence columns -------------------------------------
  
  
  # Cleaning up the EG.PrecursorId column
  spectronaut_data$EG.PrecursorId <- gsub("_", "", spectronaut_data$EG.PrecursorId)
  
  # Splitting the EG.PrecursorId column into 2 columns
  spectronaut_data[c("PeptideModSeq", "PrecursorCharge")] <- str_split_fixed(spectronaut_data$EG.PrecursorId, pattern = "\\.", n = 2)
  
  # Converting PrecursorCharge to Numeric
  spectronaut_data$PrecursorCharge <- as.numeric(spectronaut_data$PrecursorCharge)
  
  # Cleaning up the PeptideModSeq column
  spectronaut_data$PeptideModSeq <- gsub(" \\s*\\([^\\)]+\\)", "", spectronaut_data$PeptideModSeq)
  
  # Extracting Stripped sequence from PeptideModSeq
  spectronaut_data$PeptideSeq <- gsub("\\[.*?\\]", "", spectronaut_data$PeptideModSeq)
  
  
  # Finalizing table --------------------------------------------------------
  
  
  # Adding a column for how data was processed
  spectronaut_data$Software <- "Spectronaut"
  
  final_column_names <- c("Filename", "PeptideSeq",
                          "PeptideModSeq","PrecursorCharge",
                          "PrecursorMz", "RTInMin",
                          "ProteinAccession", "ProteinDescription",
                          "GeneName", "TotalIntensity",
                          "TIC", "Software")
  
  # Reorganizing columns
  spectronaut_data <- spectronaut_data[final_column_names]
  
  
  # Data output -------------------------------------------------------------
  
  
  # Final output
  return(spectronaut_data)
  
}


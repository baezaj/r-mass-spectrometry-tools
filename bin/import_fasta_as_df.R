import_fasta_as_df <- function(fname){
  
  
  # Error handling ----------------------------------------------------------
  
  
  # File must be a fasta
  if(!grepl(".fasta",fname)){
    stop(paste0("Input file must be a fasta"))
  }

  
  # Meat and potatoes -------------------------------------------------------
  
  

  
  require(seqinr)
  # Reading in the fasta file
  fasta <- read.fasta(fname,
                      seqtype = "AA",
                      set.attributes = FALSE, 
                      whole.header = TRUE)
  
  # Creating list for Protein Accession
  # This ID matches what is exported from EncyclopeDIA
  ProteinAccession <- unlist(lapply(names(fasta), function(x) unlist(strsplit(x, split = " "))[1]))
  
  # Parsing the Protein Description
  ProteinDescription <- substr(names(fasta), start = regexpr(pattern = " ", names(fasta)), stop = nchar(names(fasta)))
  ProteinDescription <- unlist(lapply(ProteinDescription, function(x) unlist(strsplit(x, split = " OS="))[1]))
  
  # Parsing the Gene name
  GeneName <- unlist(lapply(names(fasta), function(x) unlist(strsplit(x, split = "GN="))[2]))
  GeneName <- unlist(lapply(GeneName, function(x) unlist(strsplit(x, split = " PE="))[1]))
  
  # Parsing Organism
  Organism <- substr(ProteinAccession, start = regexpr("_", ProteinAccession) + 1, stop = nchar(ProteinAccession))
  
  # Creating a dataframe from the extracted information
  fasta_df <- data.frame(cbind(ProteinAccession, ProteinDescription, GeneName, Organism))
  
  # Protein Sequence
  fasta_df$ProteinSequence <- sapply(fasta, FUN = paste, collapse = "")
  
  return(fasta_df)
  
}




import_encyc_peptide_as_df <- function(elib_dir, fasta_dir){
  
  require(DBI)
  require(RSQLite)
  


  # data import -------------------------------------------------------------

  # reading fasta file
  fasta_df <- import_fasta_as_df(fasta_dir)

  ## EncyclopeDIA output
  # Connect to the SQLite database, i.e. the elib file
  con <- dbConnect(SQLite(), elib_dir)
  
  ### Fetch the peptide and protein from the elib file
  peptidequants <- dbReadTable(con, "peptidequants")
  peptidescores <- dbReadTable(con, "peptidescores")
  peptidetoprotein <- dbReadTable(con, "peptidetoprotein")
  proteinscores <- dbReadTable(con, "proteinscores")
  
  # data is fetched so disconnect it.
  dbDisconnect(con)
  
  # Organizing data ---------------------------------------------------------
  
  ######## 
  # 1. Removing Decoy peptides
  peptidescores <- peptidescores %>% 
    filter(IsDecoy == 0) %>%
    select(PeptideSeq, PeptideModSeq) 
  
  ## 2. Clean up peptide quant table
  peptidequants <- peptidequants %>% 
    select(PrecursorCharge, PeptideModSeq, PeptideSeq, TotalIntensity, SourceFile) %>% 
    inner_join(peptidescores, .)
  
  ## 3. Filter for proteins at 1% FDR
  proteinscores <- proteinscores %>% 
    filter(IsDecoy == 0) %>%
    select(ProteinGroup, ProteinAccession)
  
  ## 3. Merging PeptideSeq and ProteinAccession with ProteinGroup
  ## 4. Removing ProteinAccession
  temp <- peptidetoprotein %>% 
    filter(isDecoy == 0) %>% 
    select(ProteinAccession, PeptideSeq) %>% 
    full_join(., proteinscores) %>% 
    select(ProteinGroup, PeptideSeq) %>% 
    filter(!is.na(ProteinGroup))
  
  ## 5. Merging ProteinScore with Fasta output using ProteinAccession
  ## 6. Collapsing Protein groups
  proteinscores <- left_join(proteinscores, fasta_df) %>% 
    group_by(ProteinGroup) %>% 
    summarize(ProteinAccession = paste(ProteinAccession, collapse = "; "),
              ProteinDescription = paste(ProteinDescription, collapse = "; "),
              GeneName = paste(GeneName, collapse = "; ")) %>% 
    ungroup()
  
  ## 7. Merging PeptideSeq with ProteinGroup from the temporary dataframe
  peptidetoprotein <- peptidetoprotein %>% 
    filter(isDecoy == 0) %>%
    full_join(., temp) %>% 
    select(PeptideSeq, ProteinGroup) %>% 
    filter(!is.na(ProteinGroup)) %>%
    distinct()
  
  ## 8. Merging the PeptideSeq with Protein annotation using ProteinGroup
  peptidetoprotein <- full_join(peptidetoprotein, proteinscores)
  
  ## 9. Adding the Protein annotation to the peptidequants table 
  peptidequants <- right_join(peptidetoprotein, peptidequants)
  
  ## 10. Removing empty peptides
  peptidequants <- peptidequants %>% 
    filter(!is.na(ProteinGroup))
  
  ## 11. Cleaning up the SourceFile name
  peptidequants$SourceFile <- sub(".mzML", "", peptidequants$SourceFile)
  
  # Data output
  return(peptidequants)
  
}

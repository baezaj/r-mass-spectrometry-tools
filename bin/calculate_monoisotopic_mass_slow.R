calculate_mass <- function(sequence, IAA = TRUE, AcK = TRUE, N15 = FALSE) {
  
  
  ## calculate m/z values for resulting peptides
  residueMass <- function(residue){
    
    C <- 12.0000000                                         # carbon-12
    H <- 1.0078250321                                       # hydrogen-1
    O <- 15.9949146221                                      # oxygen-16
    S <- 31.97207069                                        # sulfer-32    
    N <- ifelse(N15 == TRUE, 15.0001088984, 14.0030740052)  # nitrogen-14 or -15
    proton <- 1.007276466
    
    
    if(residue == "A") mass = C*3 + H*5 + N + O         # alanine
    if(residue == "R") mass = C*6 + H*12 + N*4 + O      # arginine
    if(residue == "N") mass = C*4 + H*6 + N*2 + O*2     # asparagine
    if(residue == "D") mass = C*4 + H*5 + N + O*3       # aspartic acid
    if(residue == "E") mass = C*5 + H*7 + N + O*3       # glutamic acid
    if(residue == "Q") mass = C*5 + H*8 + N*2 + O*2     # glutamine
    if(residue == "G") mass = C*2 + H*3 + N + O         # glycine
    if(residue == "H") mass = C*6 + H*7 + N*3 + O       # histidine
    if(residue == "I") mass = C*6 + H*11 + N + O        # isoleucine
    if(residue == "L") mass = C*6 + H*11 + N + O        # leucine
    if(residue == "K") mass = C*6 + H*12 + N*2 + O      # lysine
    if(residue == "M") mass = C*5 + H*9 + N + O + S     # methionine
    if(residue == "F") mass = C*9 + H*9 + N + O         # phenylalanine
    if(residue == "P") mass = C*5 + H*7 + N + O         # proline
    if(residue == "S") mass = C*3 + H*5 + N + O*2       # serine
    if(residue == "T") mass = C*4 + H*7 + N + O*2       # threonine
    if(residue == "W") mass = C*11 + H*10 + N*2 + O     # tryptophan
    if(residue == "Y") mass = C*9 + H*9 + N + O*2       # tyrosine
    if(residue == "V") mass = C*5 + H*9 + N + O         # valine
    
    if(residue == "K" & AcK == FALSE) mass = C*6 + H*12 + N*2 + O*1      # unmodified lysine
    if(residue == "K" & AcK == TRUE) mass = C*8 + H*14 + N*2 + O*2      # acetylated lysine
    
    if(residue == "C" & IAA == FALSE) mass = C*3 + H*5 + N + O + S   # cysteine, iodoacetylated cysteine
    if(residue == "C" & IAA == TRUE)
      mass <- ifelse(N15 == FALSE, C*5 + H*8 + N*2 + O*2 + S,      # do not include nitrogen-15 in IAA
                     C*5 + H*8 + N + 14.0030740052 + O*2 + S)
    
    
    return(mass)
  }
  
  
  ## calculate monoisotopic mass of peptides
  peptide_vector <- strsplit(sequence, split = "")
  peptide_mass <- unlist(lapply(peptide_vector, function(x){
    round((sum(sapply(unlist(x), residueMass)) + H*2 + O), digits = 5)
  }))
  
  return(peptide_mass)
  
}



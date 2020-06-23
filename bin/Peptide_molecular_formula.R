Peptide_molecular_formula <- function(sequence, N15 = 0, IAA = TRUE, AcK = TRUE, charge = 1, 
                                  custom = list(code = NULL, elements = NULL)) {
  
  
  if(charge < 1 | charge > 3) stop("charge must be 1, 2, or 3")
  
  seq_vector <- strsplit(sequence, split = "")[[1]]
  x <- c(C = 0, H = 0, N = 0, O = 0, S = 0)
  
  for(i in 1:(length(seq_vector))) {
    if(seq_vector[i] == "A") x <- x + c(C = 3, H = 5, N = 1, O = 1, S = 0)
    if(seq_vector[i] == "R") x <- x + c(C = 6, H =12, N = 4, O = 1, S = 0)
    if(seq_vector[i] == "N") x <- x + c(C = 4, H = 6, N = 2, O = 2, S = 0)
    if(seq_vector[i] == "D") x <- x + c(C = 4, H = 5, N = 1, O = 3, S = 0)
    if(seq_vector[i] == "E") x <- x + c(C = 5, H = 7, N = 1, O = 3, S = 0)
    if(seq_vector[i] == "Q") x <- x + c(C = 5, H = 8, N = 2, O = 2, S = 0)
    if(seq_vector[i] == "G") x <- x + c(C = 2, H = 3, N = 1, O = 1, S = 0)
    if(seq_vector[i] == "H") x <- x + c(C = 6, H = 7, N = 3, O = 1, S = 0)
    if(seq_vector[i] == "I") x <- x + c(C = 6, H =11, N = 1, O = 1, S = 0)
    if(seq_vector[i] == "L") x <- x + c(C = 6, H =11, N = 1, O = 1, S = 0)
    if(seq_vector[i] == "M") x <- x + c(C = 5, H = 9, N = 1, O = 1, S = 1)
    if(seq_vector[i] == "F") x <- x + c(C = 9, H = 9, N = 1, O = 1, S = 0)
    if(seq_vector[i] == "P") x <- x + c(C = 5, H = 7, N = 1, O = 1, S = 0)
    if(seq_vector[i] == "S") x <- x + c(C = 3, H = 5, N = 1, O = 2, S = 0)
    if(seq_vector[i] == "T") x <- x + c(C = 4, H = 7, N = 1, O = 2, S = 0)
    if(seq_vector[i] == "W") x <- x + c(C =11, H =10, N = 2, O = 1, S = 0)
    if(seq_vector[i] == "Y") x <- x + c(C = 9, H = 9, N = 1, O = 2, S = 0)
    if(seq_vector[i] == "V") x <- x + c(C = 5, H = 9, N = 1, O = 1, S = 0)
    
    ## if IAA = TRUE, unlabeled N from IAA added handled separately
    if(seq_vector[i] == "C" & IAA == TRUE) x <- x + c(C = 5, H = 8, N = 1, O = 2, S = 1)
    if(seq_vector[i] == "C" & IAA == FALSE) x <- x + c(C = 3, H = 5, N = 1, O = 1, S = 1)
    
    ## if AcK = TRUE
    if(seq_vector[i] == "K" & AcK == TRUE) x <- x + c(C = 8, H = 14, N = 2, O = 2, S = 0)
    if(seq_vector[i] == "K" & AcK == FALSE) x <- x + c(C = 6, H = 12, N = 2, O = 1, S = 0)
    
  }
  
  ## add N-terminal H and C-terminal OH
  elements <- x + c(C = 0, H = 2, N = 0, O = 1, S = 0) 
  
  return(list(elements))
  
}

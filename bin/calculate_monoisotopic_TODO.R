
calculate_monoisotopic <- function(sequence, IAA = TRUE, AcK = FALSE, charge = NULL){
  
  seq_vector <- strsplit(as.character(sequence), split = "")[[1]]
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
  
  formula <- as.list(elements)
  
  defaultFormula <- list(C = 0, H = 0, N = 0, O = 0, S = 0, P = 0, Br = 0, Cl = 0, F = 0, Si = 0, x = 0)
  
  defaultFormula[names(formula)] <- formula   # replace default values with argument values
  
  defaultIsotopes <- list(C = 12.00000000, 
                          H = 1.0078250321, 
                          N = 14.0030740052, 
                          O = 15.9949146221, 
                          S = 31.97207069, 
                          P = 30.97376151#,
                          # Br = 78.9183376,
                          # Cl = 34.96885271,
                          # F = 18.99840320,
                          # Si = 27.9769265327,
                          # x = 0
                          )
  
  # defaultIsotopes[names(isotopes)] <- isotopes
  
  # if(charge < 0 & abs(charge) > defaultFormula$H)
  #   stop("the number of negative charges exceeds the number of hydrogens in the formula list")
  
  mass <- (defaultFormula$C * defaultIsotopes$C + 
             defaultFormula$H * defaultIsotopes$H +
             defaultFormula$N * defaultIsotopes$N + 
             defaultFormula$O * defaultIsotopes$O +
             defaultFormula$S * defaultIsotopes$S + 
             defaultFormula$P * defaultIsotopes$P #+
             # defaultFormula$Br * defaultIsotopes$Br +
             # defaultFormula$Cl * defaultIsotopes$Cl +
             # defaultFormula$F * defaultIsotopes$F +
             # defaultFormula$Si * defaultIsotopes$Si +
             # defaultFormula$x * defaultIsotopes$x
           )
  
  mass <- abs((mass + charge * 1.007276466) / charge)
  
  return(mass)
  
}

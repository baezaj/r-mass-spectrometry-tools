for(i in seq_along(data$fragment.ion.sequence)){
  sequence.vector <- strsplit(data$EG.StrippedSequence[i], split = "")[[1]]
  vector.length   <- length(sequence.vector)
  
  # Generates the y-ion sequence  
  if(data$F.FrgType[i] == "y"){
    data$fragment.ion.sequence[i] <- 
      paste(sequence.vector[((length(sequence.vector)-data$F.FrgNum[i])+1):
                              length(sequence.vector)], collapse = "")
  }
  
  # Generates the b-ion sequence  
  if(data$F.FrgType[i] == "b"){
    data$fragment.ion.sequence[i] <- 
      paste(sequence.vector[1:(data$F.FrgNum[i])], collapse = "")
  }
    
}

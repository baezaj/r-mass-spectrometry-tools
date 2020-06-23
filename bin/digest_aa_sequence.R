digest_aa_sequence <- function(sequence, 
                               enzyme = "trypsin", 
                               missed = 0){
  
  
  ## determine cleavage sites according to enzyme specific rules
  
  seq_vector <- strsplit(sequence, split = "")[[1]]
  end_position <- length(seq_vector)
  
  if(enzyme == "trypsin") {                                
    if(seq_vector[end_position] == "K" | seq_vector[end_position] == "R") {
      seq_vector[end_position] <- "!"
      seq_string <- paste(seq_vector, collapse = "")
    } else seq_string <- sequence    
    seq_string <- gsub("KP", "!P", seq_string)          # prevent cleavage at K and R if followed by P  
    seq_string <- gsub("RP", "!P", seq_string)     
    seq_vector <- strsplit(seq_string, split = "")[[1]]
    stop <- grep("K|R", seq_vector)
    start <- stop + 1    
  }
  
  if(enzyme == "trypsin.strict") {                                
    if(seq_vector[end_position] == "K" | seq_vector[end_position] == "R") {
      seq_vector[end_position] <- "!"
      seq_string <- paste(seq_vector, collapse = "")
    } else seq_string <- sequence
    seq_vector <- strsplit(seq_string, split = "")[[1]]
    stop <- grep("K|R", seq_vector)
    start <- stop + 1    
  }
  
  if(enzyme == "pepsin") {
    if(seq_vector[end_position] == "F" | seq_vector[end_position] == "L" |
       seq_vector[end_position] == "W" | seq_vector[end_position] == "Y" | 
       seq_vector[end_position] == "A" | seq_vector[end_position] == "E" | 
       seq_vector[end_position] == "Q") {
      seq_vector[end_position] <- "!"
    } 
    stop <- grep("F|L|W|Y|A|E|Q", seq_vector)       
    start <- stop + 1
  }
  
  if(enzyme == "arg.c") {                                
    if(seq_vector[end_position] == "R"){
      seq_vector[end_position] <- "!"
      seq_string <- paste(seq_vector, collapse = "")
    } else seq_string <- sequence    
    seq_string <- gsub("RP", "!P", seq_string)          # prevent cleavage at R if followed by P  
    seq_vector <- strsplit(seq_string, split = "")[[1]]
    stop <- grep("R", seq_vector)
    start <- stop + 1    
  }
  
  if(enzyme == "arg.c, glu.c") {                                
    if(seq_vector[end_position] == "R" | seq_vector[end_position] == "E") {
      seq_vector[end_position] <- "!"
      seq_string <- paste(seq_vector, collapse = "")
    } else seq_string <- sequence    
    seq_string <- gsub("RP", "!P", seq_string)          # prevent cleavage at R if followed by P  
    seq_vector <- strsplit(seq_string, split = "")[[1]]
    stop <- grep("R|E", seq_vector)
    start <- stop + 1    
  }
  ## error checking
  
  if(enzyme != "trypsin" & enzyme != "trypsin.strict" & enzyme != "pepsin" & enzyme != "arg.c" & enzyme != "arg.c, glu.c") 
    stop("undefined enzyme, defined enzymes are trypsin, trypsin.strict, and pepsin and arg.c, and glu.c")                    
  if(length(stop) == 0) warning("sequence does not contain cleavage sites")
  if(missed > length(stop)) stop("number of specified missed cleavages is greater than the maximum possible")
  
  
  ## cleave sequence
  
  cleave <- function(sequence, start, stop, misses) {
    peptide <- substring(sequence, start, stop)
    mc <- rep(misses, times = length(peptide))
    result <- data.frame(peptide, start, stop, mc, stringsAsFactors = FALSE)
    return(result) 
  }
  
  start <- c(1, start)                           # peptides if 0 missed cleavages
  stop <- c(stop, end_position)
  results <- cleave(sequence, start, stop, 0)
  
  if(missed > 0) {                               # peptides if missed cleavages > 0
    for(i in 1:missed) {
      start_tmp <- start[1:(length(start) - i)]
      stop_tmp <- stop[(1 + i):length(stop)]
      peptide <- cleave(sequence, start_tmp, stop_tmp, i)
      results <- rbind(results, peptide) 
    } 
  }
  return(results)
}

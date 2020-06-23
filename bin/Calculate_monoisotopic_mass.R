calculate_monoisotopic_mass <- function(seq, IAA = TRUE, AcK = FALSE) {
  
  # Split sequence by amino acids
  seq <- toupper(seq)
  seq <- gsub(pattern = "[[:space:]]+", replacement = "", x = seq)
  seq <- strsplit(x = seq, split = "")
  
  # Create the weight scale
  weight <- c(
    A = 71.037113805,
    R = 156.101111050,
    N = 114.042927470,
    D = 115.026943065,
    C = ifelse(IAA == FALSE, 103.009184505, 103.009184505 + 57.021464),
    E = 129.042593135,
    Q = 128.058577540,
    G = 57.021463735,
    H = 137.058911875,
    I = 113.084064015,
    L = 113.084064015,
    K = ifelse(AcK == FALSE, 128.094963050, 128.094963050 + 42.010565),
    M = 131.040484645,
    F = 147.068413945,
    P = 97.052763875,
    S = 87.032028435,
    T = 101.047678505,
    W = 186.079312980,
    Y = 163.063328575,
    V = 99.068413945,
    H2O = 18.01056)
  
  # Sum the weight of each amino acid and add H2O weight
  unlist(lapply(seq, function(seq){
    sum(weight[c(seq, "H2O")])
  }))
}


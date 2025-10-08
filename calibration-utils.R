log_seq <- function(initial_value, final_value, n = 100) {
  if (final_value > initial_value & n > 2 & initial_value > 0 & final_value > 0) {
    # need n-1 vs. n because the starting point is an exponent of 0 - (n-1) (n divisions)
    ratio <- (final_value / initial_value)^(1 / (n - 1))
    logseq <- initial_value * ratio^(0:(n - 1))
  } else {
    logseq <- -1
  }
  
  return(logseq)
}

fx_4pl <- function(x, parameters) {
  A <- parameters[1]
  B <- parameters[2]
  C <- parameters[3]
  D <- parameters[4]
  
  y <- NaN
  
  if (x >= 0) {
    y <- D + (A - D) / (1 + (x / C)^B)
  }
  
  return(as.numeric(y))
}

fx_inv_4pl <- function(y, parameters) {
  A <- parameters[1]
  B <- parameters[2]
  C <- parameters[3]
  D <- parameters[4]
  
  if ((A - D) / (y - D) < 1) {
    y <- A
  }
  
  x <- as.numeric(C * ((A - D) / (y - D) - 1)^(1 / B))
  
  return(x)
}

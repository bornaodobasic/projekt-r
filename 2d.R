#Simulacija u 2d pomoću Floryjevog modela

#jamming funkcija za 2d 
jamming <- function(x) {
  return(0.562009 + (0.315 / x) + (0.114 / x^2))
}

#inicijalizacija polja
initArray <- function(n, size) {
  arr <- rep(TRUE, (n - size)^2)
  return(arr)
}

#brisanje područja koje postaje nedostupno
removeArrayRegion <- function(n, array, size, samp) {
  
  sampledRow <- ceiling(samp / (n - size))
  sampledCol <- samp %% (n - size)
 
  if (sampledCol == 0) sampledCol <- n - size

  startRow <- max(1, sampledRow - size)
  endRow <- min(n - size, sampledRow + size)
  
  startCol <- max(1, sampledCol - size)
  endCol <- min(n - size, sampledCol + size)
  
  rows <- startRow:endRow
  cols <- startCol:endCol
  forRemove <- as.vector(outer(rows - 1, cols - 1, function(r, c) r * (n - size) + c + 1))
  
  forRemove <- forRemove[forRemove >= 1 & forRemove <= length(array)]
  
  array[forRemove] <- FALSE
  
  return(array)
}

#pokretanje simulacije
simArr <- function(n, array, size) {
  brojAutomobila <- 0
  
  while(any(array)) {
    available <- which(array)
    
    if (length(available) == 1) {
      samp <- available[1]
    } else {
      samp <- sample(available, 1)
    }    
    
    Sys.sleep(0.01) 
    
    array <- removeArrayRegion(n, array, size, samp)
    
    brojAutomobila <- brojAutomobila + 1
  }
  return(brojAutomobila * (size + 1)^2 / n^2)
}

  

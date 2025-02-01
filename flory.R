#Simulacija Flory modela

limitFlory = (1 - exp(-2))/2

flory <- function(n, k) {
  #uvjet kraja rekurzije
  if (k > n) {
    return(0)  
  }
  
  #bira se jedan broj između dostupnih mjesta
  x <- sample(0:(n - k), 1) 
  #x presijeca na dva dijela i postavlja 1 auto
  return(1 + flory(x, k) + flory(n - k - x, k))
}

repFlory <- function(n, k , reps) {
  result <- numeric(reps)
  
  replicate(reps, flory(n, k)) -> result
  result
}


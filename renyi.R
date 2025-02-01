#Simulacija Renyi modela

limitRenyi <- 0.7475979203

renyi <- function(n, k) {
  #uvjet kraja rekurzije
  if (k > n) {
    return(0)  
  }
  
  #bira se jedan broj u intervalu
  x <- runif(1, 0, n - k)
  #x presijeca na dva dijela i postavlja 1 auto
  return(1 + renyi(x, k) + renyi(n - k - x, k))
}

repRenyi <- function(n, k , reps) {
  result <- numeric(reps)
  
  replicate(reps, renyi(n, k)) -> result
  result
}
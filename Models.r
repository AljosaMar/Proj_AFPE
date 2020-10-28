#'Functions for the simulation of data
#'
#'
#'

models <- function(name, N){
  
  stopifnot(is.element(name, c("linear", "polynomial", "expo", "expo2","treshold") ))
  
  eps <- rnorm(N,0,0.1)
  
  if (name == "linear")  {series <- linear(eps,2,c(2,5),c(0.5,-0.5),c(0,0,0,0,0))}
  if (name == "polynomial") {series <- poly(eps,c(2,5),c(1,-1),c(2,2),c(0,0,0,0,0))}          
  if (name == "expo") {series <- expo(eps, c(0,0,0,0,0))}
  if (name == "expo2") {series <- expo2(eps, c(0,0,0,0,0))}
  if (name == "treshold") {series <- treshold(eps, c(0,0,0,0,0)) }
  
  return(series)
}

linear <- function(eps, dim, lags, coeff, initial_values) {
  
  stopifnot( dim == length(coeff) & dim == length(lags))
  x <-eps
  x[1:lags[dim]] <- initial_values
  
  for (i in (lags[dim]+1):length(eps)) {
    
    x[i] <- coeff %*% x[i-lags] + eps[i]
    
  }
  
  return(x)
  
}

poly <- function(eps, lags, coeff, exponents, initial_values) {
  
  stopifnot( length(exponents) == length(coeff) & length(exponents) == length(lags))
  x <- eps
  d <- length(lags)
  x[1:lags[d]] <- initial_values
  
  for (i in (lags[d]+1):length(eps)) {
    x[i] <-  coeff %*% x[i-lags]^exponents +  eps[i]
    
  }
  return(x)
  
}

treshold <- function(eps, initial_values){
   
  x <- initial_values
  
  for (i in (length(initial_values)+1):length(eps)){
    
    if (x[i-2] >= 0) x[i] <- 0.5*x[i-2] - 0.5*x[i-5] + eps[i]
    else x[i] <- 0.5*x[i-5]
  }
  
  return(x)
} 

expo <- function(eps, initial_values){
  
  x <- initial_values
  
  for (i in (length(initial_values)+1):length(eps)){
    x[i] <- (-0.28-0.49*exp(-3.89*(x[i-2])^2))*x[i-2] + (0.41 - 0.54 * exp(-3.89*(x[i-5])^2))*x[i-5] + eps[i]
  }
  
  return(x)
  
}
expo2 <- function(eps, initial_values){
  
  x <- initial_values
  
  for (i in (length(initial_values)+1):length(eps)){
    x[i] <- (0.5-0.5*exp(-50*(x[i-2])^2))*x[i-2] + (0.5 - 2.1 * exp(-50*(x[i-5])^2))*x[i-5] + eps[i]
  }
  
  return(x)
  
}

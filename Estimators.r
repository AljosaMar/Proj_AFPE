#
#
#
#
#
#
#

jdens <- function(series, lags, kern = "gaussian", h = 0.1 ) {
  
  d <- length(lags)
  N <- length(series)
  K <- kernel(kern,d)
  
  dist <- function(x) {
    stopifnot( length(x) == d)
    q <- 0
    
    for (j in (lags[d]+1):(N+lags[1])){
      
      q <- q+ K(x-series[j-lags],h) 
      
    }
    q <- q/(N+lags[1]-lags[d]) 
    return(q)  
    
  }  
  
  return(dist)
}


# Auxilliary function r

estr <- function(series, lags, kern = "gaussian", h = 0.1) {
  
  d <- length(lags)
  N <- length(series)
  K <- kernel(kern,d)
  
  funk <- function(x) {
    stopifnot(length(x) == d )
    q <- 0
    
    for (j in (lags[d]+1):N){
      
      q <- q+ series[j]*K(x-series[j-lags],h)
    }
    
    q <- q/(N-lags[d])
    
    return(q)  
    
  }  
  
  return(funk)
}

Con_Mean <- function(series, lags, kern = "gaussian", h = 0.1) {
  
  m  <- function(x) {
    stopifnot( length(x) == length(lags))
    H <- jdens(series,lags,kern,h)
    L <- estr(series,lags,kern,h)
    q <- L(x)/H(x)
    return(q)  
  }  
  return(m)
}

Projection <- function(series, lags, xMask, kern = "gaussian", h = 0.1) {
  
  d <- length(lags)
  N <- length(series)
  
  
  funk <- function(x) {
    stopifnot(length(xMask) == length(x))
    M <- Con_Mean(series,lags,kern,h)
    w <- 0
    
    for (j in (lags[d]+1):N) {
      Q <- series[j-lags]
      u <- replace(Q,xMask,x)
      w <- w + M(u)
      
    }
    w <- w/(N-lags[d])
    
    
    return(w)  
    
  }  
  
  return(funk)
  
}

# Auxilliary function R

estR <- function(series, lags, kern = "gaussian", h = 0.1) {
  
  d <- length(lags)
  N <- length(series)
  K <- kernel(kern,d)
  
  funk <- function(x) {
    stopifnot(length(x) == d )
    q <- 0
    
    for (j in (lags[d]+1):N){
      
      q <- q+ (series[j])^2 * K(x-series[j-lags],h)
    }
    
    q <- q/(N-lags[d])
    
    return(q)  
    
  }  
  
  return(funk)
}

Con_Var <- function(series, lags, kern = "gaussian", h = 0.1) {
  
  m  <- function(x) {
    stopifnot( length(x) == length(lags))
    H <- jdens(series,lags,kern,h)
    L <- estR(series,lags,kern,h)
    q <- L(x)/H(x)
    return(q)  
  }  
  return(m)
}

Var_Proj <- function(series, lags, xMask, kern = "gaussian", h = 0.1) {
  
  d <- length(lags)
  N <- length(series)
  
  
  funk <- function(x) {
    stopifnot(length(xMask) == length(x))
    M <- Con_Var(series,lags,kern,h)
    w <- 0
    
    for (j in (lags[d]+1):N) {
      Q <- series[j-lags]
      u <- replace(Q,xMask,x)
      w <- w + M(u)
      
    }
    w <- w/(N-lags[d])
    
    
    return(w)  
    
  }  
  
  return(funk)
  
}

Sigma_M_est <- function(series, lags, kern = "gaussian", h = 0.1) {
  
  d <- length(lags)
  
  funk <- function(x) {
    stopifnot( length(x) == d)
    p <- jdens(series, lags, kern, h)
    
    L2 <- K_L2(kern, d)
    
    q <- 0.01 * L2 / p(x)
    return(q)
  }
  
  return(funk)
}


#
#
#
#

A_est <- function(series, lags, kern = "gaussian"){
   
    m <- length(lags)
    N <- length(series)
    
    funk <- function(h){
      M <- Con_Mean(series, lags, kern, h)
      q <- 0
    
    for (i in (lags[m]+1):N) {
        q <- q + (series[i] - M(series[i-lags]))^2  
    }
      q <- q/(N-lags[m]+1)
      return(q)
    }
    
    return(funk)
}

B_est <- function(series, lags, kern = "gaussian"){
    
    m <- length(lags)
    N <- length(series)
  
    funk <- function(h){
      M <- Con_Mean(series, lags, kern, h)
      p <- jdens(series,lags,kern,h)
      q <- 0
      
      for (i in (lags[m]+1):N) {
       
          q <- q + (series[i] - M(series[i-lags]))^2 / p(series[i-lags])  
      }
      q <- q/(N-lags[m]+1)
      return(q)
  }
  
  return(funk)
}

AFPE_opt <- function(series, lags, kern = "gaussian"){
  
  A <- A_est(series, lags, kern)
  B <- B_est(series, lags, kern)
  K <- kernel(kern, 1)
  m <- length(lags)
  N <- length(series)
  
  o1 <- optimise(A,c(0.0005,50))
  o2 <- optimise(B,c(0.0005,50))
  
  q <- o1$objective + 2* (K(0))^m * o2$objective/((N-lags[m]+1) * (o1$minimum)^m)
  
  res <- c(q,o1$minimum)
  return(res)
  
}


AFPE_combn <- function(series, m, k) {
  
  
  AFPE <- list()
  pick <- list()
  h <- list()
  for (i in 1:m) {
    s <- choose(k,i)
    S <- combn(1:k,i)
    for (j in 1:s) {
      res <-  AFPE_opt_3(series,lags = S[,j])
      AFPE <- c(AFPE,res[1])
      h <- c(h, res[2])
      pick <- append(pick, list(S[,j]))
    }
  }
  
  AFPE_df <- list(FPE = AFPE, lags = pick, band = h)
  return(AFPE_df)
}

AFPE_proc <- function(series, m, k) {
  
  fpe_list <- AFPE_combn_3(series,m,k)
  
  min <- which.min(fpe_list$FPE)
  
  return(list(FPE = fpe_list$FPE[min] , lags = fpe_list$lags[min], band =  fpe_list$h[min]))
  
}

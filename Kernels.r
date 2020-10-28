#'Product kernel
#'
#'The function returns a product kernel for the specified dimension and kernel type
#'
#'@param type a string out of c("gaussian","rectangular", "triangular", "parabolic", 
#'"biweight") 
#'@param dim positiv integer

kernel <- function(type = "gaussian", dim = 1){
  
  stopifnot(is.element(type, c("gaussian","rectangular", "triangular", "parabolic", 
                              "biweight")) )
    res <- switch (type,
                   "gaussian"    = gaussian_kernel,
                   "rectangular" = rectangular_kernel,
                   "triangular"  = triangular_kernel, 
                   "parabolic"   = parabolic_kernel,
                   "biweight"    = biweight_kernel
    )
    
    p <- 1
    
    pom <- function(x,h = 0.1) {
      if (length(x) == dim) {
        
        for (i in 1:dim) {
          p <- p*(1/h)*res(x[i]/h)
        }}
      else print(FALSE)
      return(p)
    }
    
    return(pom)
}


# commonly used kernels
gaussian_kernel <- function(x){
  1/sqrt(2*pi) * exp(-x^2/2)
}

rectangular_kernel <- function(x){
  1/2 * Indicator(x)
}

triangular_kernel <- function(x){
  (1 - abs(x)) * Indicator(x)
}

parabolic_kernel <- function(x){
  3/4 * (1 - x^2) * Indicator(x)
}

biweight_kernel <- function(x){
  15/16 * (1 - x^2)^2 * Indicator(x)
}

# Indicator function I(|x| <= 1)
Indicator <- function(x){
  ifelse(abs(x) <= 1, 1, 0)
}

# L2-norm squared for the chosen kernel function and dimension
K_L2 <- function(kern = "gaussian", dim = 1 ) {
  
  k <- kernel(kern, 1)
  
  k_2 <- function(x) { return((k(x))^2) }
  
  I <- integrate(Vectorize(k_2), lower = -Inf, upper = Inf)$value
  
  return(I^dim)
  
}

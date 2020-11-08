# Danke Chen!
# Hvala Paula i Milane!
#
library(ggplot2)
library(cubature)

res_M1 <- list()
mean_M1 <-c()
sigma_M1 <- list()
mean_sigma_M1 <- c()

res_M2 <- list()
mean_M2 <-c()
sigma_M2 <- list()
mean_sigma_M2 <- c()


res_M3 <- list()
mean_M3 <-c()
sigma_M3 <- list()
mean_sigma_M3 <- c()

res_M4 <- list()
mean_M4 <-c()
sigma_M4 <- list()
mean_sigma_M4 <- c()

# The lists with results for Projections
res_P1 <- list()
mean_P1 <-c()
sigma_P1 <- list()
mean_sigma_P1 <- c()

res_P2 <- list()
mean_P2 <-c()
sigma_P2 <- list()
mean_sigma_P2 <- c()

res_P3 <- list()
mean_P3 <-c()
sigma_P3 <- list()
mean_sigma_P3 <- c()

res_P4 <- list()
mean_P4 <-c()
sigma_P4 <- list()
mean_sigma_P4 <- c()


v <- seq(-0.6,0.6,0.05)
N <- 2000

#One-dimensional conditional mean functions for the linear model
for (i in 1:10) {
  
  eps <- rnorm(N,0,0.1)
  
  X <-linear(eps,2,c(2,5),c(0.5,-0.5),c(0,0,0,0,0))
  
  h <- 0.05
  
  M1 <- Con_Mean(X,c(1), "gaussian", h)
  sigma_1 <- Sigma_M_est(X,c(1), "gaussian", h)
  M2 <- Con_Mean(X,c(2), "gaussian", h)
  sigma_2 <- Sigma_M_est(X,c(2), "gaussian", h)
  M3 <- Con_Mean(X,c(4), "gaussian", h)
  sigma_3 <- Sigma_M_est(X,c(4), "gaussian", h)
  M4 <- Con_Mean(X,c(5), "gaussian", h)
  sigma_4 <- Sigma_M_est(X,c(5), "gaussian", h)
  
  res_M1[[i]] <- sapply(v, M1)
  sigma_M1[[i]] <- sapply(v, sigma_1)

  res_M2[[i]] <- sapply(v, M2)
  sigma_M2[[i]] <- sapply(v, sigma_2)
  
  res_M3[[i]] <- sapply(v, M3)
  sigma_M3[[i]] <- sapply(v, sigma_3)
  
  res_M4[[i]] <- sapply(v, M4)
  sigma_M4[[i]] <- sapply(v, sigma_4)
}

mean_M1 <- Reduce(`+`, res_M1) / 10
mean_sigma_M1 <- Reduce(`+`, sigma_M1) /10
sigma_M1_minus <- mean_M1 - 1.96 * mean_sigma_M1
sigma_M1_plus <- mean_M1 + 1.96 * mean_sigma_M1

mean_M2 <- Reduce(`+`, res_M2) / 10
mean_sigma_M2 <- Reduce(`+`, sigma_M2) /10
sigma_M2_minus <- mean_M2 - 1.96 * mean_sigma_M2
sigma_M2_plus <- mean_M2 + 1.96 * mean_sigma_M2

mean_M3 <- Reduce(`+`, res_M3) / 10
mean_sigma_M3 <- Reduce(`+`, sigma_M3) /10
sigma_M3_minus <- mean_M3 - 1.96 * mean_sigma_M3
sigma_M3_plus <- mean_M3 + 1.96 * mean_sigma_M3

mean_M4 <- Reduce(`+`, res_M4) / 10
mean_sigma_M4 <- Reduce(`+`, sigma_M4) /10
sigma_M4_minus <- mean_M4 - 1.96 * mean_sigma_M4
sigma_M4_plus <- mean_M4 + 1.96 * mean_sigma_M4

data <- data.frame("x" = v, "M1" = res_M1,"sigma_M1_minus" = sigma_M1_minus, "sigma_M1_plus" = sigma_M1_plus,
                   "M2" = res_M2,"sigma_M2_minus" = sigma_M2_minus, "sigma_M2_plus" = sigma_M2_plus, 
                   "M3" = res_M3,"sigma_M3_minus" = sigma_M3_minus, "sigma_M3_plus" = sigma_M3_plus,
                   "M4" = res_M4,"sigma_M4_minus" = sigma_M4_minus, "sigma_M4_plus" = sigma_M4_plus)


plot1 <- as.character("ggplot(data, aes(x = x))")
plot2 <- as.character("ggplot(data, aes(x = x))")
plot3 <- as.character("ggplot(data, aes(x = x))")
plot4 <- as.character("ggplot(data, aes(x = x))")

for (i in 1:10) {    
  
  plot1 <- paste(plot1,"+geom_line(aes(y = res_M1[[",as.character(i) ,"]]))")
  plot2 <- paste(plot2,"+geom_line(aes(y = res_M2[[",as.character(i) ,"]]))")
  plot3 <- paste(plot3,"+geom_line(aes(y = res_M3[[",as.character(i) ,"]]))")
  plot4 <- paste(plot4,"+geom_line(aes(y = res_M4[[",as.character(i) ,"]]))")
  
}


plot1 <- paste(plot1,"+geom_line(aes(y = sigma_M1_minus), color = \"green\", size = 1.2)+geom_line(aes(y = sigma_M1_plus), color = \"green\", size = 1.2) +
               xlim(-0.4,0.4) + ylim(-0.8,0.8) + 
               labs(x = \" x \" , y = \"M1\", title = \" Conditional Means for the linear model, sample size 200 \")")
plot2 <- paste(plot2,"+geom_line(aes(y = sigma_M2_minus), color = \"green\", size = 1.2)+geom_line(aes(y = sigma_M2_plus), color = \"green\", size = 1.2) +
               xlim(-0.4,0.4) + ylim(-0.8,0.8) + 
               labs(x = \" x \" , y = \"M2\")")
plot3 <- paste(plot3,"+geom_line(aes(y = sigma_M3_minus), color = \"green\", size = 1.2)+geom_line(aes(y = sigma_M3_plus), color = \"green\", size = 1.2) +
               xlim(-0.4,0.4) + ylim(-0.8,0.8) + 
               labs(x = \" x \" , y = \" M4 \") ")
plot4 <- paste(plot4,"+geom_line(aes(y = sigma_M4_minus), color = \"green\", size = 1.2)+geom_line(aes(y = sigma_M4_plus), color = \"green\", size = 1.2) +
               xlim(-0.4,0.4) + ylim(-0.8,0.8) + 
               labs(x = \" x \" , y = \" M5 \")")

eval(parse(text = plot1))
eval(parse(text = plot2))
eval(parse(text = plot3))
eval(parse(text = plot4))


# The same simulation for different models, but projectors and their confidence intervals are calculated
u <- seq(-0.6,0.6,0.05)
N <- 1000
for (i in 1:10) {
  
  eps <- rnorm(N,0,0.1)
  
  X <-poly(eps,c(2,5),c(1,-1), c(2,2), c(0,0,0,0,0))
  
  h <- 0.05
  
  P1 <- Projection(X,c(1,2,4,5), c(1), "gaussian", h)
  sigma_1 <- Sigma_P_est(X,c(1,2,4,5),c(1), "gaussian", h)
  
  P2 <- Projection(X,c(1,2,4,5), c(2), "gaussian", h)
  sigma_2 <- Sigma_P_est(X,c(1,2,4,5),c(2), "gaussian", h)
  
  P3 <- Projection(X,c(1,2,4,5), c(3), "gaussian", h)
  sigma_3 <- Sigma_P_est(X,c(1,2,4,5),c(3), "gaussian", h)
  
  P4 <- Projection(X,c(1,2,4,5), c(4), "gaussian", h)
  sigma_4 <- Sigma_P_est(X,c(1,2,4,5),c(4), "gaussian", h)
  
  res_P1[[i]] <- sapply(v, P1)
  sigma_P1[[i]] <- sapply(u, sigma_1)
  
  res_P2[[i]] <- sapply(v, P2)
  sigma_P2[[i]] <- sapply(u, sigma_2)
  
  res_P3[[i]] <- sapply(v, P3)
  sigma_P3[[i]] <- sapply(u, sigma_3)
  
  res_P4[[i]] <- sapply(v, P4)
  sigma_P4[[i]] <- sapply(u, sigma_4)
}

mean_P1 <- Reduce(`+`, res_P1) / 10
mean_sigma_P1 <- Reduce(`+`, sigma_P1) /10
sigma_P1_minus <- mean_P1 - 1.96 * mean_sigma_P1
sigma_P1_plus <- mean_P1 + 1.96 * mean_sigma_P1

mean_P2 <- Reduce(`+`, res_P2) / 10
mean_sigma_P2 <- Reduce(`+`, sigma_P2) /10
sigma_P2_minus <- mean_P2 - 1.96 * mean_sigma_P2
sigma_P2_plus <- mean_P2 + 1.96 * mean_sigma_P2

mean_P3 <- Reduce(`+`, res_P3) / 10
mean_sigma_P3 <- Reduce(`+`, sigma_P3) /10
sigma_P3_minus <- mean_P3 - 1.96 * mean_sigma_P3
sigma_P3_plus <- mean_P3 + 1.96 * mean_sigma_P3

mean_P4 <- Reduce(`+`, res_P4) / 10
mean_sigma_P4 <- Reduce(`+`, sigma_P4) /10
sigma_P4_minus <- mean_P4 - 1.96 * mean_sigma_P4
sigma_P4_plus <- mean_P4 + 1.96 * mean_sigma_P4

data <- data.frame("x" = v, "P1" = res_P1,"sigma_P1_minus" = sigma_P1_minus, "sigma_P1_plus" = sigma_P1_plus,
                   "P2" = res_P2,"sigma_P2_minus" = sigma_P2_minus, "sigma_P2_plus" = sigma_P2_plus, 
                   "P3" = res_P3,"sigma_P3_minus" = sigma_P3_minus, "sigma_P3_plus" = sigma_P3_plus,
                   "P4" = res_P4,"sigma_P4_minus" = sigma_P4_minus, "sigma_P4_plus" = sigma_P4_plus)


plot1 <- as.character("ggplot(data, aes(x = x))")
plot2 <- as.character("ggplot(data, aes(x = x))")
plot3 <- as.character("ggplot(data, aes(x = x))")
plot4 <- as.character("ggplot(data, aes(x = x))")

for (i in 1:10) {    
  
  plot1 <- paste(plot1,"+geom_line(aes(y = res_P1[[",as.character(i) ,"]]))")
  plot2 <- paste(plot2,"+geom_line(aes(y = res_P2[[",as.character(i) ,"]]))")
  plot3 <- paste(plot3,"+geom_line(aes(y = res_P3[[",as.character(i) ,"]]))")
  plot4 <- paste(plot4,"+geom_line(aes(y = res_P4[[",as.character(i) ,"]]))")
  
}


plot1 <- paste(plot1,"+geom_line(aes(y = sigma_P1_minus), color = \"green\", size = 1.2)+geom_line(aes(y = sigma_P1_plus), color = \"green\", size = 1.2) +
               xlim(-0.5,0.5) + ylim(-0.3,0.3) + 
               labs(x = \" x \" , y = \"P1\", title = \" Projections for the linear model, sample size 1000 \")")
plot2 <- paste(plot2,"+ geom_line(aes(y = sigma_P2_minus), color = \"green\", size = 1.2)+geom_line(aes(y = sigma_P2_plus), color = \"green\", size = 1.2) +
               xlim(-0.5,0.5) + ylim(-0.3,0.3) + 
               labs(x = \" x \" , y = \"P2\")")
plot3 <- paste(plot3,"+geom_line(aes(y = sigma_P3_minus), color = \"green\", size = 1.2)+geom_line(aes(y = sigma_P3_plus), color = \"green\", size = 1.2) +
               xlim(-0.5,0.5) + ylim(-0.3,0.3) + 
               labs(x = \" x \" , y = \" P3 \") ")
plot4 <- paste(plot4,"+geom_line(aes(y = sigma_P4_minus), color = \"green\", size = 1.2)+geom_line(aes(y = sigma_P4_plus), color = \"green\", size = 1.2) +
               xlim(-0.5,0.5) + ylim(-0.3,0.3) + 
               labs(x = \" x \" , y = \" P4 \")")

eval(parse(text = plot1))
eval(parse(text = plot2))
eval(parse(text = plot3))
eval(parse(text = plot4))

# Example of the deteriation of the projections with dimension 
N <- 400
v <- seq(-0.5,0.5,0.05)
for (i in 1:10) {
  
  eps <- rnorm(N,0,0.1)
  
  X <-poly(eps,c(2,5),c(1,-1),c(2,2), c(0,0,0,0,0))
  
  h <- 0.05
  
  P1 <- Projection(X,c(2,5), c(1), "gaussian", h)
  
  P2 <- Projection(X,c(2,5), c(2), "gaussian", h)

  
  res_P1[[i]] <- sapply(v, P1)

  res_P2[[i]] <- sapply(v, P2)


}
data <- data.frame("x" = v, "P1" = res_P1, "P2" = res_P2)


plot1 <- as.character("ggplot(data, aes(x = x))")
plot2 <- as.character("ggplot(data, aes(x = x))")
for (i in 1:10) {  
  
  plot1 <- paste(plot1,"+geom_line(aes(y = res_P1[[",as.character(i) ,"]]))")
  plot2 <- paste(plot2,"+geom_line(aes(y = res_P2[[",as.character(i) ,"]]))")
  
}
plot1 <- paste(plot1,"+ xlim(-0.5,0.5) + ylim(-0.3,0.3) + labs(x = \" x \" , y = \"P1\")")
plot2 <- paste(plot2," + xlim(-0.5,0.5) + ylim(-0.3,0.3) + labs(x = \" x \" , y = \"P2\")")

eval(parse(text = plot1))
eval(parse(text = plot2))

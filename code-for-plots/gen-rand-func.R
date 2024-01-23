## construct some simple GP priors 
source("gpfunc.R")
lin.kern.basic <- function(x0, x1, b){
  b[1] + b[2]*(x0 + x1) + b[3]*x0*x1
}
lin.covmat <- function(x1, x2,b){
  res <- matrix(ncol = length(x1) ,nrow = length(x2))
  for( i in 1:length(x1) ){
    
    for( j in 1:length(x2) ){
      res[i,j] <- lin.kern.basic(x1[i], x2[j], b)
    }
      
  }
  res
}
nn <- 200
x0 <- rep(0, nn)
x1 <- seq(0,5, length=nn)
theta.vec <- c(0.1, 0.5, 1, 2)
par(mfrow=c(1,1))
plot(x1, exp(-cor.kern.fun(x0, x1,w = theta.vec[1])), type = "l", xlab = "||x - x'||",
     ylab = "Correlation", main = "Squared Exponential Correlation Function")
for(i in 2:4){
  lines(x1, exp(-cor.kern.fun(x0, x1,w = theta.vec[i])), type = "l", col = i)
}
legend(x=4,y=1, lwd = 2, col = 1:4, legend = theta.vec, title = "theta",bty="n")

## construct random functions for each lengthscale ...

set.seed(1140)## 1140am, 26th nov 2021
par(mfrow = c(2,2))
for(i in 1:4){
  
  Kx <- cov.x1.x2(x1, x1,1, theta.vec[i],2)
  ytmp <- MASS::mvrnorm(1, rep(0, nn), Kx)
  plot(x1, ytmp, type = "l", ylim = c(-4,4),main = paste("theta = ",theta.vec[i]),
       xlab = "x", ylab = "f(x)")
  for(j in 1:4){
    ytmp <- MASS::mvrnorm(1, rep(0, nn), Kx)
    lines(x1, ytmp, type = "l")
  }
  
  
}
## linear kernel
set.seed(1606)## 15:59  26th nov 2021
par(mfrow = c(1,1))
b <- c(1, -0.3, 1)
x1 <- seq(-1,1,length=30)
my.lin.covmat <- lin.covmat(x1, x1, b)

ytmp <- MASS::mvrnorm(1, rep(0, length(x1)), my.lin.covmat)
plot(x1, ytmp, type = "l", ylim = c(-4,4),
     xlab = "x", ylab = "f(x)", main = "Function Draws: Linear Kernel")
for(j in 1:4){
  ytmp <- MASS::mvrnorm(1, rep(0, length(x1)), my.lin.covmat)
  lines(x1, ytmp, type = "l")
}
## mixed kernel
set.seed(1400)## 14:00  29th nov 2021
par(mfrow = c(1,1))
b <- c(1, -0.3, 1)
x1 <- seq(-1,1,length=30)
my.lin.covmat <- lin.covmat(x1, x1, b)/2
my.se.covmat <- cov.x1.x2(x1, x1,1, 1 ,2)/2
my.covmat <- my.lin.covmat + my.se.covmat
ytmp <- MASS::mvrnorm(1, rep(0, length(x1)), my.covmat)
plot(x1, ytmp, type = "l", ylim = c(-4,4),
     xlab = "x", ylab = "f(x)", main = "Function Draws: Mixed Kernel")
for(j in 1:4){
  ytmp <- MASS::mvrnorm(1, rep(0, length(x1)),  my.covmat)
  lines(x1, ytmp, type = "l")
}

# # simple plots of marginal behavour ...
source("gpfunc.R")
lin.kern.basic <- function(x0, x1, b){
  b[1] + b[2]*(x0 + x1) + b[3]*x0*x1
}
lin.covmat <- function(x1, x2,b){
  res <- matrix(nrow = length(x1) , ncol = length(x2))
  for( i in 1:length(x1) ){
    
    for( j in 1:length(x2) ){
      res[i,j] <- lin.kern.basic(x1[i], x2[j], b)
    }
    
  }
  res
}

post.band <- function(x, m, v, multiplier, clr = NULL){
  
  lo <- m - multiplier*sqrt(v)
  hi <- m + multiplier*sqrt(v)
  
  if(is.null(clr)){
    clr <- rgb(1,0,0,alpha = 0.3)
  }
  
  polygon(c((x), rev(x)), c((lo), rev(hi)), col = clr, border = "transparent")
}
f <- function(x){
 2*sinpi(x) + cospi(3*x) + 3*x
}
x <- seq(0, 1, length=100)*2 - 1
plot(x, f(x), type = "l")

## training data

## compute posterior ...
set.seed(123456)
#mai=  c(1.02, 0.82, 0.82, 0.42)
par(mfrow=c(1,2), mai = c(1.02, 0.72, 0.82, 0.25))
xtrain <- runif(5)*2 - 1
ytrain <- f(xtrain)
plot(xtrain, ytrain, pch=19, ylim = c(-20,20), xlim  = c(-1,1))

## compute data mean and covmat...
h <- function(x) cbind(1, x)
bmean <- rep(0,2)
sig <- 1; theta <- 0.3
b.pars <- c(2,0,2)
var.dat <- cov.x1.x2(xtrain, xtrain, sig, theta, 2) + diag(1e-6, length(ytrain))
var.dat <- var.dat + lin.covmat(xtrain, xtrain, b.pars)
mean.dat <- h(xtrain) %*% bmean

## compute prior mean and covmat ...
prior.var <- cov.x1.x2(x, x, sig, theta, 2)

prior.var <- prior.var + lin.covmat(x, x, b.pars) 
prior.mean <- h(x) %*% bmean
plot(x, prior.mean, type = "l", ylim = c(-10, 10), xlim  = c(-1,1),
     xlab = "x", ylab = "f(x)", main= "Prior Distribution for f(x)")
## compute prior marginal variances ...
post.band(x, prior.mean, diag(prior.var), 3)
post.band(x, prior.mean, diag(prior.var), 2)
post.band(x, prior.mean, diag(prior.var), 1)
lines(x,prior.mean, lwd=1.5)
lines(x, f(x), col = "blue", lwd = 1.5)
legend(0.2,-3, legend= c("Truth", "Mean", "Uncertainty"),
       lwd = 2, col = c("blue", "black", "red"), bty="n",  y.intersp = 1.5)
## compute posterior ...
precmat <- chol2inv(chol(var.dat))
cov.pred.dat <- cov.x1.x2(x, xtrain, sig, theta, 2) + lin.covmat(x, xtrain, b.pars)
post.mean <- prior.mean + cov.pred.dat %*% precmat %*% (ytrain - mean.dat)
post.var <- prior.var - cov.pred.dat %*% precmat %*% t(cov.pred.dat)


plot(x, post.mean, type = "l", ylim = c(-10,10), xlim  = c(-1,1),
     xlab = "x", ylab = "", main= "Posterior for f(x)")
title(ylab = "f(x)", line=2)
## compute prior marginal variances ...

post.band(x, post.mean, diag(post.var), 3)
post.band(x, post.mean, diag(post.var), 2)
post.band(x, post.mean, diag(post.var), 1)
lines(x,post.mean, lwd=1.5)

lines(x, f(x), col = "blue", lwd =1.5)
points(xtrain, ytrain, pch=20)
#legend(0.5,-5, legend= c("Truth", "Mean", "Uncertainty"),
#       lwd = 2, col = c("blue", "black", "red"), bty="n",  y.intersp = 1.5)
## change width of x
x <- seq(-10, 10, length=400)
prior.var <- cov.x1.x2(x, x, sig, theta, 2)

prior.var <- prior.var + lin.covmat(x, x, b.pars) 
prior.mean <- h(x) %*% bmean
if(F){
  plot(x, prior.mean, type = "l", ylim = c(-10, 10), xlim  = range(x),
       xlab = "x", ylab = "f(x)", main= "Prior Distribution for f(x)")
  ## compute prior marginal variances ...
  post.band(x, prior.mean, diag(prior.var), 3)
  post.band(x, prior.mean, diag(prior.var), 2)
  post.band(x, prior.mean, diag(prior.var), 1)
  lines(x,prior.mean, lwd=1.5)
  lines(x, f(x), col = "blue", lwd = 1.5)
  legend(0.5,-5, legend= c("Truth", "Mean", "Uncertainty"),
         lwd = 2, col = c("blue", "black", "red"), bty="n",  y.intersp = 1.5)
}

## compute posterior ...
precmat <- chol2inv(chol(var.dat))
cov.pred.dat <- cov.x1.x2(x, xtrain, sig, theta, 2) + lin.covmat(x, xtrain, b.pars)
post.mean <- prior.mean + cov.pred.dat %*% precmat %*% (ytrain - mean.dat)
post.var <- prior.var - cov.pred.dat %*% precmat %*% t(cov.pred.dat)

plot(x, post.mean, type = "l", ylim = c(-100,100), xlim  = range(x),
     xlab = "x", ylab = "", main= "Posterior for f(x)")
## compute prior marginal variances ...

post.band(x, post.mean, diag(post.var), 3)
post.band(x, post.mean, diag(post.var), 2)
post.band(x, post.mean, diag(post.var), 1)
lines(x,post.mean, lwd=1.5)

lines(x, f(x), col = "blue", lwd =1.5)
points(xtrain, ytrain, pch=20, cex=0.5)
legend(-2,-40, legend= c("Truth", "Mean", "Uncertainty"),
       lwd = 2, col = c("blue", "black", "red"), bty="n",  y.intersp = 1.5)

##  now a stochastic code (same as before + noise)
f.noisy <- function(x, lambda = 0.5) f(x) + rnorm(length(x))*lambda
set.seed(654321)
xtrain <- runif(30)*2 - 1
ynoisy <- f.noisy(xtrain)
plot(xtrain, ynoisy,pch=19)
x <- seq(0, 1, length=100)*2 - 1
h <- function(x) cbind(1, x)
bmean <- rep(0,2)
sig <- 1; theta <- 0.3; lambda <- 0.5
b.pars <- c(2,0,2)
var.dat <- cov.x1.x2(xtrain, xtrain, sig, theta, 2) + diag(lambda^2, length(ynoisy))
var.dat <- var.dat + lin.covmat(xtrain, xtrain, b.pars)
mean.dat <- h(xtrain) %*% bmean

## compute prior mean and covmat ...
prior.var <- cov.x1.x2(x, x, sig, theta, 2)

prior.var <- prior.var + lin.covmat(x, x, b.pars) 
prior.mean <- h(x) %*% bmean
par(mfrow=c(1,1))
plot(x, prior.mean, type = "l", ylim = c(-10, 10), xlim  = c(-1,1),
     xlab = "x", ylab = "f(x)", main= "Prior Distribution for f(x)")
## compute prior marginal variances ...
post.band(x, prior.mean, diag(prior.var), 3)
post.band(x, prior.mean, diag(prior.var), 2)
post.band(x, prior.mean, diag(prior.var), 1)
lines(x,prior.mean, lwd=1.5)
lines(x, f(x), col = "blue", lwd = 1.5)
legend(0,-5, legend= c("Truth", "Mean", "Uncertainty"),
       lwd = 2, col = c("blue", "black", "red"), bty="n",  y.intersp = 1.5)
## compute posterior ...
precmat <- chol2inv(chol(var.dat))
cov.pred.dat <- cov.x1.x2(x, xtrain, sig, theta, 2) + lin.covmat(x, xtrain, b.pars)
post.mean <- prior.mean + cov.pred.dat %*% precmat %*% (ynoisy - mean.dat)
post.var.f <- prior.var - cov.pred.dat %*% precmat %*% t(cov.pred.dat)
post.var.y <- post.var.f  + diag(lambda^2, length(post.mean))


par(mfrow = c(1,1))
plot(x, post.mean, type = "l", ylim = c(-10,10), xlim  = c(-1,1),
     xlab = "x", ylab = "f(x)", main= "Stochastic Emulator")
## compute prior marginal variances ...
post.band(x, post.mean, diag(post.var.f), 2)
lines(x,post.mean - 2*sqrt(diag(post.var.y)), lty = 2)
lines(x,post.mean + 2*sqrt(diag(post.var.y)), lty = 2)

#post.band(x, post.mean, diag(post.var.f), 3)

#post.band(x, post.mean, diag(post.var.f), 1)
lines(x,post.mean, lwd=1.5)

lines(x, f(x), col = "blue", lwd =1.5)
points(xtrain, ynoisy, pch=20)
lines(x, f(x) - 2*lambda, col = "blue", lty = 2, lwd =1.5)
lines(x, f(x) + 2*lambda, col = "blue", lty = 2, lwd =1.5)
#legend(0.5,-5, legend= c("Truth", "Mean", "Uncertainty"),
#       lwd = 2, col = c("blue", "black", "red"), bty="n",  y.intersp = 1.5)

#plot(x, post.mean, type = "l", ylim = c(-10,10), xlim  = c(-1,1),
#     xlab = "x", ylab = "y(x)", main= "Small Sample")
## compute prior marginal variances ...
# post.band(x, post.mean, diag(post.var.y), 3)
# post.band(x, post.mean, diag(post.var.y), 2)
# post.band(x, post.mean, diag(post.var.y), 1)
# lines(x,post.mean, lwd=1.5)
# 
# lines(x, f(x), col = "blue", lwd =1.5)
# lines(x, f(x) - lambda, col = "blue", lwd = 1, lty = 2)
# lines(x, f(x) + lambda, col = "blue", lwd = 1, lty = 2)
# 
# lines(x, f(x) - 2*lambda, col = "blue", lwd = 1, lty = 3)
# lines(x, f(x) + 2*lambda, col = "blue", lwd = 1, lty = 3)
# 
# lines(x, f(x) - 3*lambda, col = "blue", lwd = 1, lty = 4)
# lines(x, f(x) + 3*lambda, col = "blue", lwd = 1, lty = 4)
# 
# points(xtrain, ynoisy, pch=20)
# legend(0.5,-5, legend= c("Truth", "Mean", "Uncertainty"),
#        lwd = 2, col = c("blue", "black", "red"), bty="n",  y.intersp = 1.5)
#  

### observe more data ...
set.seed(987654321)
xtrain <- runif(300)*2 - 1
ynoisy <- f.noisy(xtrain)
#plot(xtrain, ynoisy,pch=19)
x <- seq(0, 1, length=100)*2 - 1
h <- function(x) cbind(1, x)
bmean <- rep(0,2)
sig <- 1; theta <- 0.3; lambda <- 0.5
b.pars <- c(2,0,2)
var.dat <- cov.x1.x2(xtrain, xtrain, sig, theta, 2) + diag(lambda^2, length(ynoisy))
var.dat <- var.dat + lin.covmat(xtrain, xtrain, b.pars)
mean.dat <- h(xtrain) %*% bmean

## compute prior mean and covmat ...
prior.var <- cov.x1.x2(x, x, sig, theta, 2)

prior.var <- prior.var + lin.covmat(x, x, b.pars) 
prior.mean <- h(x) %*% bmean
#plot(x, prior.mean, type = "l", ylim = c(-10, 10), xlim  = c(-1,1),
#     xlab = "x", ylab = "f(x)", main= "Prior Distribution for f(x)")
## compute prior marginal variances ...
#post.band(x, prior.mean, diag(prior.var), 3)
#post.band(x, prior.mean, diag(prior.var), 2)
#post.band(x, prior.mean, diag(prior.var), 1)
#lines(x,prior.mean, lwd=1.5)
#lines(x, f(x), col = "blue", lwd = 1.5)
#legend(0.5,-5, legend= c("Truth", "Mean", "Uncertainty"),
#       lwd = 2, col = c("blue", "black", "red"), bty="n",  y.intersp = 1.5)
## compute posterior ...
precmat <- chol2inv(chol(var.dat))
cov.pred.dat <- cov.x1.x2(x, xtrain, sig, theta, 2) + lin.covmat(x, xtrain, b.pars)
post.mean <- prior.mean + cov.pred.dat %*% precmat %*% (ynoisy - mean.dat)
post.var.f <- prior.var - cov.pred.dat %*% precmat %*% t(cov.pred.dat)
post.var.y <- post.var.f  + diag(lambda^2, length(post.mean))


#par(mfrow = c(1,2))
plot(x, post.mean, type = "l", ylim = c(-10,10), xlim  = c(-1,1),
     xlab = "x", ylab = "f(x)", main= "f - Large Sample")
points(xtrain, ynoisy, pch = 19)
## compute prior marginal variances ...
## compute prior marginal variances ...
lines(x,post.mean - 2*sqrt(diag(post.var.f)), lty = 2)
lines(x,post.mean + 2*sqrt(diag(post.var.f)), lty = 2)

#post.band(x, post.mean, diag(post.var.f), 3)
post.band(x, post.mean, diag(post.var.y), 2)
#post.band(x, post.mean, diag(post.var.f), 1)
lines(x,post.mean, lwd=1.5)

lines(x, f(x), col = "blue", lwd =1.5)
#points(xtrain, ynoisy, pch=20)
lines(x, f(x) - 2*lambda, col = "blue", lty = 2, lwd =1.5)
lines(x, f(x) + 2*lambda, col = "blue", lty = 2, lwd =1.5)
legend(0.5,-5, legend= c("Truth", "Mean", "Uncertainty"),
       lwd = 2, col = c("blue", "black", "red"), bty="n",  y.intersp = 1.5)


plot(x, post.mean, type = "l", ylim = c(-10,10), xlim  = c(-1,1),
     xlab = "x", ylab = "y(x)", main= "Small Sample")
post.band(x, post.mean, diag(post.var.f), 3)
post.band(x, post.mean, diag(post.var.f), 2)
post.band(x, post.mean, diag(post.var.f), 1)
lines(x,post.mean, lwd=1.5)

lines(x, f(x), col = "blue", lwd =1.5)
points(xtrain, ynoisy, pch=20)
legend(0.5,-5, legend= c("Truth", "Mean", "Uncertainty"),
       lwd = 2, col = c("blue", "black", "red"), bty="n",  y.intersp = 1.5)


plot(x, post.mean, type = "l", ylim = c(-10,10 ), main  = "y - Large Sample")
## compute prior marginal variances ...

post.band(x, post.mean, diag(post.var.y), 3)
post.band(x, post.mean, diag(post.var.y), 2)
post.band(x, post.mean, diag(post.var.y), 1)
lines(x,post.mean, lwd=1.5)

lines(x, f(x), col = "blue", lwd =1.5)
lines(x, f(x) - lambda, col = "blue", lwd = 1, lty = 2)
lines(x, f(x) + lambda, col = "blue", lwd = 1, lty = 2)

lines(x, f(x) - 2*lambda, col = "blue", lwd = 1, lty = 3)
lines(x, f(x) + 2*lambda, col = "blue", lwd = 1, lty = 3)

lines(x, f(x) - 3*lambda, col = "blue", lwd = 1, lty = 4)
lines(x, f(x) + 3*lambda, col = "blue", lwd = 1, lty = 4)

points(xtrain, ynoisy, pch=20)
legend(0.5,-5, legend= c("Truth", "Mean", "Uncertainty"),
       lwd = 2, col = c("blue", "black", "red"), bty="n",  y.intersp = 1.5)
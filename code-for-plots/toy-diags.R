## diagnostics code ...
source("gpfunc.R")
f <- function(x){
  
  top <-  2300*x[1]^3 + 1900*x[1]^2 + 2092*x[1] + 60
  btm <- 100*x[1]^3 + 500*x[1]^2 + 4*x[1] + 20
  res <- (1 - exp(-0.5*1/x[2])) * top/btm
  res
  
}
coverage <- function(resids){
  alpha <- seq(0, 1, length = length(resids))
  emp.cov <- rep(0, length(resids))
  for(i in 1:length(resids)){
    emp.cov[i] <- sum(abs(resids) < qnorm(1-alpha[i]/2))/length(resids)
  }
  list(x = 1 - alpha,y=emp.cov) 
}
#set.seed(1138)##11:38am, 14th dec 2021
#set.seed(1541)
set.seed(1812)
x <- lhs::randomLHS(20, 2)
x.test <- lhs::randomLHS(20,2)
y <- vector(length = dim(x)[1])
y.test <- vector(length = dim(x.test)[1])
for(i in 1:dim(x)[1]){
  y[i] <- f(x[i,])
}
for(i in 1:dim(x.test)[1]){
  y.test[i] <- f(x.test[i,])
}
par(mfrow=c(1,2))
plot(x[,1], y);plot(x[,2], y)
theta <- c(0.2421, 0.4240); sig <- 3.3316^0.5; lam <- 1e-8
h <- function(x) cbind(1, x)
b <- rep(0,3)
B <- diag(100,length(b))

dat.covmat <- cov.x1.x2(x, x, sig, theta, 2) + (h(x)) %*% B %*% t(h(x)) + diag(lam^2,dim(x)[1])
dat.precmat <- chol2inv(chol(dat.covmat))
cov.pred.dat <- cov.x1.x2(x.test, x, sig, theta, 2) + (h(x.test)) %*% B %*% t(h(x))
prior.var <- cov.x1.x2(x.test, x.test, sig, theta, 2) + (h(x.test)) %*% B %*% t(h(x.test))
m <- h(x.test)%*%b + cov.pred.dat %*% dat.precmat %*% (y - h(x)%*%b)
v <- prior.var -  cov.pred.dat %*% dat.precmat %*% t(cov.pred.dat)
## cholesky errors
chol.v <- t(chol(v))
chol.errs <- forwardsolve(chol.v, y.test - m)
std.errs <- (y.test - m)/sqrt(diag(v))
par(mfrow = c(1,2))
max.err <- max(abs(chol.errs))
par(mfrow=c(2,2))
for(i in 1:2){
  plot(x.test[,i], chol.errs, pch=19, ylim = c(-max.err,max.err)*1.1,
       ylab = "C.E.s", xlab = paste("x",i,sep=""),main = paste("C.E.s vs x",i,sep=""))
       abline(h = c(-1.96,1.96), lty = 2, col = 2)
}

## coverage plot
obs.coverage <- coverage(chol.errs)
plot(obs.coverage, pch = 19, ylab = "Observed Coverage",xlab = "Expected Coverage"); abline(0,1)
n.test <- length(y.test)
for(i in 1:100) points(coverage(rnorm(n.test)), col = rgb(1,0,0,alpha = 0.1),pch=19)
points(obs.coverage,pch=19)

qqnorm(chol.errs, pch=19);abline(0,1)
####

## update emulator
n.new <- 10
x.new <- lhs::randomLHS(n.new,2)*0.5
x <- rbind(x, x.new)
par(mfrow=c(1,2))
y.new <- rep(0, n.new)
for(i in 1:n.new) y.new[i] <- f(x.new[i,])
y <- c(y, y.new)
plot(x[,1], y);plot(x[,2], y)
theta <- c(0.1764, 0.4116); sig <- 4.9389^0.5; lam <- 1e-8
h <- function(x) cbind(1, x)
b <- rep(0,3)
B <- diag(100,length(b))

dat.covmat <- cov.x1.x2(x, x, sig, theta, 2) + (h(x)) %*% B %*% t(h(x)) + diag(lam^2,dim(x)[1])
dat.precmat <- chol2inv(chol(dat.covmat))
cov.pred.dat <- cov.x1.x2(x.test, x, sig, theta, 2) + (h(x.test)) %*% B %*% t(h(x))
prior.var <- cov.x1.x2(x.test, x.test, sig, theta, 2) + (h(x.test)) %*% B %*% t(h(x.test))
m <- h(x.test)%*%b + cov.pred.dat %*% dat.precmat %*% (y - h(x)%*%b)
v <- prior.var -  cov.pred.dat %*% dat.precmat %*% t(cov.pred.dat)
## cholesky errors
chol.v <- t(chol(v))
chol.errs <- forwardsolve(chol.v, y.test - m)
std.errs <- (y.test - m)/sqrt(diag(v))
max.err <- max(abs(chol.errs))
par(mfrow=c(2,2))
for(i in 1:2){
  plot(x.test[,i], chol.errs, pch=19, ylim = c(-max.err,max.err)*1.1,
       ylab = "C.E.s", xlab = paste("x",i,sep=""),main = paste("C.E.s vs x",i,sep=""))
  abline(h = c(-1.96,1.96), lty = 2, col = 2)
}

## coverage plot
obs.coverage <- coverage(chol.errs)
plot(obs.coverage, pch = 19, ylab = "Observed Coverage",xlab = "Expected Coverage"); abline(0,1)
n.test <- length(y.test)
for(i in 1:100) points(coverage(rnorm(n.test)), col = rgb(1,0,0,alpha = 0.1),pch=19)
points(obs.coverage, pch=19)
abline(0,1)

qqnorm(chol.errs, pch=19);abline(0,1)

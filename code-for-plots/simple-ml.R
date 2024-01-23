## solve and ODE w/multilevel emulator ...
post.band <- function(x, m, v, multiplier, clr = NULL){
  
  lo <- m - multiplier*sqrt(v)
  hi <- m + multiplier*sqrt(v)
  
  if(is.null(clr)){
    clr <- rgb(1,0,0,alpha = 0.3)
  }
  
  polygon(c((x), rev(x)), c((lo), rev(hi)), col = clr, border = "transparent")
}
euler.method <- function(f.init, f.grad, x.init, x.final, dx){
  xgrid <- seq(from = x.init, to = x.final, by = dx)

  n <- length(xgrid)-1
  fgrid <- rep(NA,n+1)
  fgrid[1] <- f.init
  x.curr <- x.init
  for(i in 1:n){
    x.curr <- xgrid[i+1]
    fgrid[i+1] <- fgrid[i] + dx*f.grad(x.curr)
  }
  rev(fgrid)[1]
}

## solve dy/dx = cos(x) for various x_0
## trial run
par(mfrow=c(1,1))
nn <- 50
gamma.range <- seq(from =0, to = 1, length=nn)
f.final.grid <- rep(0, nn)
X <- 30
f <- function(x, gamma) exp(10*gamma^(1/2)*cospi(2*x/365)+10*(1-gamma^(1/2))*sinpi(6*x/365))
f.prime <- function(x, gamma) 10*(2*pi/365)*(3*(1-gamma^(1/2))*cospi(6*x/365)-gamma^(1/2)*sinpi(2*x/365))*exp(10*gamma^(1/2)*cospi(2*x/365)+10*(1-gamma^(1/2))*sinpi(6*x/365))
solveODE <- function(Y.init, X.init, X.final, gamma.range, dX){
  f.final.grid <- rep(0, length(gamma.range))
  for(j in 1:length(gamma.range)){
    f2.prime <- function(x) f.prime(x, gamma.range[j])
    f.final.grid[j] <- euler.method(Y.init, f2.prime, X.init, X.final,dX)
  }
  f.final.grid
}
my.integrate <- function(x, f, n){
  tt <- seq(0, 1, length = n)
  mean(f(tt, x))
}
f <- function(t, x){
 cospi(4*x^2)*sinpi(2*t*x) -4*x*t +1
}
nn <- 100
my.integrate.vec <- Vectorize(my.integrate, vectorize.args = "x")
gamma.range <- seq(from =0, to = 1, length=nn)
plot(gamma.range, my.integrate.vec(gamma.range, f, 1000), type = "l", ylim = c(-1.2,1.2),
     main = "Function Evaluations at Increasing Fidelity Levels", xlab = "x", ylab = expression( "f"["R"]~"(x ; T)" ))
nrange <- c(2,4,8,16)
for(j in 1:4) lines(gamma.range, my.integrate.vec(gamma.range, f, nrange[j]),lty = 2, col=j+1)
legend("bottomleft", col = c(1, 2:5), lty = c(1, rep(2, 5)), legend = c("analytic", nrange), title = "T", lwd = c(1,rep(1, 5)), bty="n")

#for(j in 1:length(denom))lines(gamma.range, solveODE(0, 0, X, gamma.range, X/denom[j])*1e-4, col = j+1, lty = 2)
#denom2 <- c(1, denom)
legend("bottomleft", col = c(1, 1:5), lty = c(1, rep(2, 5)), legend = c("analytic", X/denom2), title = "time step", lwd = c(2,rep(1, 5)), bty="n")
#lines(gamma.range, f(X, gamma.range) - exp(10*gamma.range), col = 2) ## ground truth
x.init.grid <- seq(from = 0, to = pi, length=nn)
f.final.grid2 <- rep(0, nn)
for(j in 1:nn){
  f.final.grid2[j] <- euler.method(f(x.init.grid[j]), f.prime, x.init.grid[j], x.init.grid[j] + X,X/100)
}
### simple ml emulator with gstat ...
source("gpfunc.R")
set.seed(1659)

n.e <- 3
n.c <- 20
x.e <- lhs::maximinLHS(n.e, 1)
x.c <- lhs::augmentLHS(x.e, n.c - n.e)

y.c <- my.integrate.vec(x.c, f, 4)
y.e <- my.integrate.vec(x.e, f, 40)

plot(x.c, y.c, xlim = c(0,1), ylim = c(-1.2, 1.2))
points(x.e, y.e, pch = 19, col=2)
xx <- seq(0,1,length=100)
lines(gamma.range, my.integrate.vec(gamma.range, f, 4))
lines(gamma.range, my.integrate.vec(gamma.range, f, 40))
############
par(mfrow=c(1,2))
  plot(x.e, y.e, xlim = c(0,1), ylim = c(-2, 2), main = "Standard Emulator",
       pch=19, ylab = "f(x; T = 40)",xlab="x")
  ## construct just a GP on the expensive data ...
  sig <- sqrt(1/2); theta <- 0.2; lam <- 1e-6; beta <- 0
  m <- cond.mean(y.e, x.e, xx, sig, theta, beta, beta, lam, 2)
  v <- diag(cond.covmat(y.e, x.e, xx, sig, theta , lam))-lam^2 
  lines(xx, m)
  post.band(xx, m, v, 2)
  lines(xx, m)
  lines(xx, my.integrate.vec(xx, f, 40), col = "cyan")
  ## construct ml emulator ...
## assume rho = 1
y.all <- c(y.c, y.e)
H.all <- rbind(cbind(rep(2, n.c-n.e),0),cbind(rep(2, n.e), rep(0.1, n.e)))
x.all <- c(x.c, x.e)
sig0 <- 1/2; theta0 <- 0.2; lam0 <- 1e-5; beta0 <- 0
sig <- 1/2; theta <- 0.25; lam <- 1e-5; beta <- 0
rho <- 1
cov.cheap <- cov.x1.x2(x.c, x.c, sig0, theta0, 2) + diag(lam0^2, n.c)
cov.exp <- cov.x1.x2(x.e, x.e, rho*sig0, theta0, 2) + cov.x1.x2(x.e, x.e, sig, theta, 2) + diag(lam^2, n.e)
crosscov <- rho*cov.x1.x2(x.e, x.c, sig0, theta0, 2)
covmat <- cbind(rbind(cov.cheap, crosscov), rbind(t(crosscov), cov.exp))
covmat
dim(covmat)
cov.pred.dat <- cbind(cov.x1.x2(xx, x.c, sig0, theta0, 2)
  ,cov.x1.x2(xx, x.e, rho*sig0, theta0, 2) + cov.x1.x2(xx, x.e, sig, theta, 2)
  )
ml.precmat <- chol2inv(chol(covmat))
plot(x.e, y.e, pch = 19, ylim = c(-2,2), xlim = c(0,1), main = "Multilevel Emulator", ylab="", xlab="x")
points(x.c, y.c, pch=3)
ml.m <- rho*beta0 + beta + cov.pred.dat %*% ml.precmat %*% ( y.all - c(rep(beta0, n.c), rep(rho*beta0 + beta, n.e)) )
lines(xx, ml.m)
ml.v <- diag(sig0^2 + sig^2 - cov.pred.dat %*% ml.precmat %*% t(cov.pred.dat))
post.band(xx, ml.m, ml.v, 2)
lines(xx,my.integrate.vec(xx, f, 40) , col = "cyan")
legend("topright", pch = c(19,3), legend = c(40,4), title = "T", bty="n")
legend("bottomleft", lty = c(1, 1, 1), col = c("cyan", "black", rgb(1,0,0,alpha = 0.3))
       ,legend = c("Truth", "Mean", "m+-2sd"), bty="n")
ytest <- my.integrate.vec(xx, f, 40)
## compute score and MSE (from a reference set ...)
sqrt(mean((ytest - m)^2))
sqrt(mean((ytest - ml.m)^2))

score <- function(y, m, v){
  -((y-m)^2)/v - log(v)
}
mean(score(ytest, m, v))
mean(score(ytest, ml.m, ml.v))

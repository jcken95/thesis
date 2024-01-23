## bayesopt acquisition functions example
source("gpfunc.R")
set.seed(9630369)
### main function
f <- function(x) {
  0.6 * sinpi(2*x) + 2*cospi(3*x) + sinpi(4*x) + 2*x
}
x <- seq(0, 1, length = 500)
par(mfrow = c(1,1))
plot(x, f(x), type  = "l", ylim = c(-3,5),
     main = "Emulated function to be maximised")
xtrain <- lhs::randomLHS(4, 1)
y <- f(xtrain)
points(xtrain, y, pch = 19)
sig = 0.75*sd(y); lambda = 1e-8; theta = 0.1; beta = mean(y)
m <- cond.mean(y, xtrain, x, sig, theta, beta, beta, lambda, 2)
v <- diag(cond.covmat(y, xtrain, x, sig, theta, lambda, 2)) - lambda^2
sd.y <- sqrt(v)
upr <- m + 2*sd.y
lwr <- m - 2*sd.y
polygon(c(x, rev(x)), c(lwr, rev(upr)), col =  rgb(1,0,0,alpha = 0.3), border = NA)
lines(x, m, col = "red", lwd = 2)
lines(x, f(x))
points(xtrain, y, pch = 19)
legend("bottomright", lty = c(1,1,1), col = c(1, "red", rgb(1, 0, 0, alpha = 0.3)),
       legend = c("True f", "mean", "mean +/-2sd"), 
       bty = "n")
## plot aq fns
ymax  = max(y)
par(mfrow = c(2,2))
PI <- ifelse(
  x %in% xtrain,
  0,
  pnorm((m - ymax)/sd.y)
)
EI <- ifelse(x %in% xtrain,
             0,
             sd.y * dnorm((m - ymax)/sd.y) + (m - ymax)*PI
)
UCB <- matrix(nrow = 3, ncol = length(x))
for(i in 1:3) {
  UCB[i,] <- m + i * sd.y
}
V <- cond.covmat(y, xtrain, x, sig, theta, lambda, 2) - diag(lambda^2, length(x))
TS <- MASS::mvrnorm(3, m, V)
par(mfrow = c(2,2))
plot(x, PI, type = "l", main = "PI", ylab = "PI",
     ylim  = c(0, 0.6))
abline(v = x[which.max(PI)])
points(x[which.max(PI)], max(PI), pch = 20)

plot(x, EI, type = "l", main = "EI", ylab = "EI",
     ylim = c(0, 0.2))
abline(v = x[which.max(EI)])
points(x[which.max(EI)], max(EI), pch = 20)
plot(x, UCB[1,], type = "l", ylim = c(-1.5, 6.5), main = "UCB", ylab = "UCB",)
for(i in 2:3) lines(x, UCB[i,], col = i)
for(i in 1:3) abline(v = x[which.max(UCB[i,])], col = i)
for(i in 1:3) points(x[which.max(UCB[i,])], max(UCB[i,]), col = i, pch = 20)

plot(x, TS[1,], type = "l", main = "TS", ylab = "Plausible functions",
     ylim = c(-1.5, 4))
for(i in 2:3) lines(x, TS[i,], col = i)
for(i in 1:3) abline(v = x[which.max(TS[i,])], col= i)
for(i in 1:3) points(x[which.max(TS[i,])], max(TS[i,]), col = i, pch = 20)

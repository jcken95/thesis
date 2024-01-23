## example HM optim
source("gpfunc.R")
f = function(x) {
  2 - sum(x^2)
}
noise = function(n, sigma) {
  rnorm(n)*sigma
}
n  = 4000
k = 2
set.seed(987)

X = lhs::randomLHS(n,  k)*2 - 1

par(mfrow = c(2,2))
#### ROUND 1
n_train = 10
x1 = lhs::randomLHS(n_train, k)*2 - 1
y1 = rep(NA, n_train)
for(i in 1:n_train) y1[i] = f(x1[i,]) + noise(1, 0.1)
## emulate
sig1 = sqrt(sd(y1)^2 - 0.1^2)
theta = c(1, 1)
lam  = 0.1
#### within sample
within_mean = cond.mean(y1, x1, x1, sig1, theta, 0, 0, lam, 2)
within_var = diag(cond.covmat(y1, x1, x1, sig1, theta, lam, 2)) - 0.1^2
#### prediction
y_mean = cond.mean(y1, x1, X, sig1, theta, 0, 0, lam, 2)
y_var = diag(cond.covmat(y1, x1, X, sig1, theta, lam, 2))  - lam^2
y_best = list(m = max(within_mean), v  = within_var[which.max(within_mean)])
I1 = (y_best$m - y_mean)/sqrt(y_var + y_best$v)
clr = rep(0, length(n))
for(i in 1:n) clr = ifelse(I1  < 3, "green", "red")
plot(X, col  = clr, main = "Wave 1",
     xlab = "x1", ylab = "x2")
points(x1, pch = 19)
points(x1[which.max(within_mean),1], x1[which.max(within_mean),2],  pch = 4)
## size of NROY 
remaining_volume = mean(I1 < 3)


#### ROUND 2
n_cand = ceiling(10/remaining_volume)
x2 = lhs::randomLHS(n_cand, k)*2 - 1
test_mean = cond.mean(y1, x1, x2, sig1, theta, 0, 0, lam, 2)
test_var = diag(cond.covmat(y1, x1, x2, sig1, theta, lam, 2)) - lam^2

I_test = (y_best$m - test_mean)/sqrt(test_var + y_best$v)
x2 = x2[I_test < 3, ]
n_train = nrow(x2)
y2 = rep(NA, n_train)

for(i in 1:n_train) y2[i] = f(x2[i,]) + noise(1, 0.1)
## emulate
sig2 = sqrt(sd(y2)^2 - 0.1^2)
theta = c(1, 1)
lam  = 0.1
#### within sample
within_mean = cond.mean(y2, x2, x2, sig2, theta, 0, 0, lam, 2)
within_var = cond.covmat(y2, x2, x2, sig2, theta, lam, 2)
#### prediction
y_mean = cond.mean(y2, x2, X, sig2, theta, 0, 0, lam, 2)
y_var = diag(cond.covmat(y2, x2, X, sig2, theta, lam, 2))  - lam^2
y_best1 = y_best
y_best = list(m = max(within_mean), v  = within_var[which.max(within_mean)])
I2 = (y_best$m - y_mean)/sqrt(y_var + y_best$v)
clr = rep(0, length(n))
for(i in 1:n){
  clr = ifelse(I1  < 3, 
               ifelse(I2  < 3, "green", "red"),
               rgb(0, 0, 0, alpha = 0.7))
} 
plot(X, col  = clr, main = "Wave 2",
     xlab = "x2", ylab = "x2")
points(x2, pch = 19)

points(x2[which.max(within_mean),1], x2[which.max(within_mean),2],  pch = 4)
## size of NROY 
remaining_volume = mean(I2 < 3)

#### ROUND 3
n_cand = ceiling(10/remaining_volume)
x3 = lhs::randomLHS(n_cand, k)*2 - 1
test_mean1 = cond.mean(y1, x1, x3, sig1, theta, 0, 0, lam, 2)
test_var1 = diag(cond.covmat(y1, x1, x3, sig1, theta, lam, 2)) - lam^2
test_mean2 = cond.mean(y2, x2, x3, sig2, theta, 0, 0, lam, 2)
test_var2 = diag(cond.covmat(y2, x2, x3, sig2, theta, lam, 2)) - lam^2
I_test1 = (y_best1$m - test_mean1)/sqrt(test_var1 + y_best1$v)
I_test2 = (y_best$m - test_mean2)/sqrt(test_var2 + y_best$v)
I_test = ifelse(I_test1 < 3,
                 I_test2,
                 I_test1)
x3 = x3[I_test < 3, ]
n_train = nrow(x3)
y3 = rep(NA, n_train)

for(i in 1:n_train) y3[i] = f(x3[i,]) + noise(1, 0.1)
## emulate
sig3 = sqrt(sd(y3)^2 - 0.1^2)
theta = c(1, 1)
lam  = 0.1
#### within sample
within_mean = cond.mean(y3, x3, x3, sig3, theta, 0, 0, lam, 2)
within_var = cond.covmat(y3, x3, x3, sig3, theta, lam, 2)
#### prediction
y_mean = cond.mean(y3, x3, X, sig3, theta, 0, 0, lam, 2)
y_var = diag(cond.covmat(y3, x3, X, sig3, theta, lam, 2))  - lam^2
y_best2  = y_best
y_best = list(m = max(within_mean), v  = within_var[which.max(within_mean)])
I3 = (y_best$m - y_mean)/sqrt(y_var + y_best$v)
clr = rep(0, length(n))
for(i in 1:n){
  clr = ifelse(I1  < 3, 
               ifelse(I2  < 3, 
                      ifelse(I3  < 3, "green", "red"),
                      rgb(0, 0, 0, alpha = 1)),
               rgb(0, 0, 0, alpha = 0.7))
} 
plot(X, col  = clr, main = "Wave 3",
     xlab = "x3", ylab = "x3")
points(x3, pch = 19)

points(x3[which.max(within_mean),1], x3[which.max(within_mean),2],  pch = 4)
## size of NROY 
remaining_volume = mean(I3 < 3)

#### ROUND 4
n_cand = ceiling(10/remaining_volume)
x4 = lhs::randomLHS(n_cand, k)*2 - 1
test_mean1 = cond.mean(y1, x1, x4, sig1, theta, 0, 0, lam, 2)
test_var1 = diag(cond.covmat(y1, x1, x4, sig1, theta, lam, 2)) - lam^2
test_mean2 = cond.mean(y2, x2, x4, sig2, theta, 0, 0, lam, 2)
test_var2 = diag(cond.covmat(y2, x2, x4, sig2, theta, lam, 2)) - lam^2
test_mean3 = cond.mean(y3, x3, x4, sig3, theta, 0, 0, lam, 2)
test_var3 = diag(cond.covmat(y3, x3, x4, sig3, theta, lam, 2)) - lam^2
I_test1 = (y_best1$m - test_mean1)/sqrt(test_var1 + y_best1$v)
I_test2 = (y_best$m - test_mean2)/sqrt(test_var2 + y_best$v)
I_test3 = (y_best$m - test_mean3)/sqrt(test_var3 + y_best$v)
I_test = ifelse(I_test1 < 3,
                ifelse(I_test2 < 3,
                       I_test3,
                       I_test2),
                I_test1)
x4 = x4[I_test < 3, ]
n_train = nrow(x4)
y4 = rep(NA, n_train)

for(i in 1:n_train) y4[i] = f(x4[i,]) + noise(1, 0.1)
## emulate
sig4 = sqrt(sd(y4)^2 - 0.1^2)
if(is.na(sig4)) {
  sig4 = sd(y4)
}
theta = c(1, 1)
lam  = 0.1
#### within sample
within_mean = cond.mean(y4, x4, x4, sig4, theta, 0, 0, lam, 2)
within_var = diag(cond.covmat(y4, x4, x4, sig4, theta, lam, 2)) - lam^2
#### prediction
y_mean = cond.mean(y4, x4, X, sig4, theta, 0, 0, lam, 2)
y_var = diag(cond.covmat(y4, x4, X, sig4, theta, lam, 2))  - lam^2
y_best3  = y_best
y_best = list(m = max(within_mean), v  = within_var[which.max(within_mean)])
I4 = (y_best$m - y_mean)/sqrt(y_var + y_best$v)
clr = rep(0, length(n))
for(i in 1:n){
  clr = ifelse(I1  < 3, 
               ifelse(I2  < 3, 
                      ifelse(I3  < 3, 
                             ifelse(I4 < 3 , "green", "red"),
                             rgb(0, 0, 0, alpha = 1)),
                      rgb(0, 0, 0, alpha = 0.7)),
               rgb(0, 0, 0, alpha = 0.5))
} 
plot(X, col  = clr, main = "Wave 4",
     xlab = "x1", ylab = "x2")
points(x4, pch = 19)

points(x4[which.max(within_mean),1], x4[which.max(within_mean),2],  pch = 4)
## size of NROY 
remaining_volume = mean(I4 < 3)

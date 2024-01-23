log_relative_volume = function(n) {
  0.5*n*log(pi) - lgamma(1 + 0.5*n) - n*log(2)
}
rv = function(n) { 
  pi^(0.5*n)/((2^n) * gamma(1 + 0.5*n))
  }

nn = seq(1, 10000, length = 100)

par(mfrow = c(1,2))
plot(nn, exp(log_relative_volume(nn)), type = "l",
     xlab = "Number input dimensions",
     ylab = "RV",
     main = "Relative volume")
plot(nn, log_relative_volume(nn), type = "l",
     xlab = "Number input dimensions",
     ylab = "log RV",
     main = "log(Relative volume)")
## linear regression
      nn = 1:20
y = log_relative_volume(nn)
linmod = lm(y ~ nn + I(nn^2))
summary(linmod)
actual = 2.926
cumulative = cumsum(exp(log_relative_volume(nn)))
100*(cumulative - actual)/actual

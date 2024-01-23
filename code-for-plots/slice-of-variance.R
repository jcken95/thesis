## slice plot for emulator ...
n = 20
y = matrix(ncol = 10, nrow = n)
for(i in 1:nrow(y)) {
  for(j in 1:ncol(y)) {
    location  = paste0("matlab-data/out-new227/OutputOuter", j, "-", i, ".mat")
    tmp = R.matlab::readMat(location)
    y[i,j]  = qnorm( mean( tmp$y[[j]][[1]] ) )
  }
}

x = seq(from = 0.1, to = 5, length = n)
log_var_hat <- log(apply(y, 1, var))
plot(x, log_var_hat, pch = 20)
abline(lm(log_var_hat ~  x))
summary(lm(log_var_hat ~  (x)))

#36, 42, 53, 56, 63. 88, 92, 100, 108, 110, 121, 144, 147, 165, 169, 177, 180, 193, 203, 208,  227
plot(x, log_var_hat, pch = 20,
     main = "Estimated Log Variance of Probit Availability",
     ylab  = "Log variance of probit availability",
     xlab = "Time to degredation (generator)")
abline(lm(log_var_hat ~  x))

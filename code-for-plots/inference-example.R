## bayesian inference example + frequentist example
## assume f known exactly

library(rstan)

f <- function(x) {
  if( x <= 1 & x >= 0 ) {
    res <- 0.6 * sinpi(2*x) + 2*cospi(3*x) + sinpi(4*x) + 2*x
  } else {
    res <- NULL
  }
  res
}
f.vec <- Vectorize(f)
xx = seq(0,1,length=1000)

## first numerically maximise
opt <- optim(xx[which.max(f.vec(xx))], fn = function(x) -1*f.vec(x),
      method  = "Brent",  lower  = 1e-8, upper = 1-1e-8)
ymax <- -opt$value
plot(xx, f.vec(xx), type = "l")
stan_data = list(
  y_obs = ymax,
  sigma = 1
)

thin  = 10000
iter = thin * 10000 * 2
start.time <- Sys.time()
stan_res = rstan::stan(file = "inference.stan",
                       data = stan_data,
                       warmup = 0.5*iter,
                       iter = iter,
                       thin = thin,
                       chains = 2)
#control = list(adapt_delta = 0.995)
end.time <- Sys.time()
end.time - start.time
stan_res
res <- as.array(stan_res)
hist(res[,1,1])
par(mfrow = c(1,2))
plot(ts(res[,1,1]), main = "Trace plot",
     ylab = "x", xlab = "Iteration")## first maximise f()
hist(res[,1,1],  main = "Posterior density",
     xlab = "x")


## simple simulator 
f = function(x) {  
  if(x <= 1 & x >=0 ){
    return(sinpi(4*x))
  }else{
    return(NULL)
  }
}
y = function(x, sig  = 1) {
  f(x) + sig * rnorm(1)
}
#data
y_obs = 2.5
var_y = 0.1

## frequentist estimate
obj_fn = function(x, y) {
  if( x < 1| x > 0) {
    res = (y - f(x))^2
  } else{
    res = 10^10
    }
  res
}
obj_fn_vec = Vectorize(obj_fn, vectorize.args = "x")
optim(par = runif(1), fn = obj_fn, y = y_obs )
xx = seq(0, 1, length = 100)
plot(xx, obj_fn_vec(xx, y_obs), type = "l")

stan_data = list(
  y =  y_obs,
  sigma =  1
)
target = 10000
thin = 1000
warmup = thin*target
mcmc_out = rstan::stan(
  file = "sin.stan", data = stan_data,
  chains = 1 , iter = 2*warmup, warmup = warmup, thin = thin)

mcmc = as.array(mcmc_out)[,,1]
acf(mcmc)
par(mfrow = c(1,2))
  plot.ts(mcmc, ylim = c(0,1),
          xlab = "Iteration",
          ylab = "x",
          main = "Trace Plot",
          xlim = c(0, length(mcmc)))
  hist(mcmc, main = "Histogram of
       Posterior Samples",
       xlab = "x",
       xlim = c(0,1))




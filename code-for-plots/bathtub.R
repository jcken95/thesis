bathtub <- function(t, alpha, kappa, time_breaks) {
  if(t < time_breaks[1]) {
    h <- alpha[1]*kappa[1]*(alpha[1]*t)^(kappa[1] - 1) 
    return(h)
  }else if(t < time_breaks[2] & t >= time_breaks[1]) {
    h <- alpha[2]
    return(h)
  }else {
    h <- alpha[3]*kappa[3]*(alpha[3]*t)^(kappa[3] - 1) 
    return(h)
  }
}


survival <-  function(t_max, alpha, kappa, time_breaks) {
  exp(
    -integrate(bathtub_vec, lower = 0, upper = t_max,
               alpha = alpha, kappa = kappa, time_breaks = time_breaks)$value)
}


time_breaks <- c(1,2,3)
kappa <- c(0.6, 1, 6)
alpha <- rep(0,3)
alpha[1] <- 0.1
alpha[2] <- alpha[1]*kappa[1]*(alpha[1]*time_breaks[1])^(kappa[1] - 1) 
alpha[3] <- (alpha[2] / (kappa[3] * time_breaks[2]^(kappa[3] - 1)))^(1/kappa[3])
time_axis <- seq(from = 0, to = max(time_breaks), length = 200)
bathtub_vec <- Vectorize(FUN = bathtub, vectorize.args = "t")
par(mfrow = c(1,2))
plot(time_axis, bathtub_vec(time_axis, alpha, kappa, time_breaks ), type = "l",
     xlab = "Time", ylab  = "h(t)", main = "Bathtub hazard function")
abline(v = time_breaks[1:2], lty = 2, col = "red")
## plot the survival function

survival_vec = Vectorize(FUN = survival, vectorize.args = "t_max")
plot(time_axis[-1], survival_vec(time_axis[-1], alpha, kappa, time_breaks ), type = "l",
     xlab = "Time", ylab  = "S(t) = P(T > t)", main = "Bathtub survival function")
abline(v = time_breaks[1:2], lty = 2, col = "red")

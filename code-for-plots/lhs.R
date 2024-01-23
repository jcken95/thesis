# lhs script...


set.seed(838312) ## 8:36 am, 3rd december
nn <- 8
rand.sample <- matrix(runif(2*nn), ncol = 2)

lh.sample <- lhs::randomLHS(nn, 2)
par(mfrow = c(1,2))
plot(rand.sample, pch = 19, ylim = c(0,1),
     xlim = c(0,1), xlab = "x1", ylab = "x2",
     main = "Uniform Sampling")
abline(0, 1, lty = 2)
plot(lh.sample, pch = 19, ylim = c(0,1), 
     xlim = c(0,1), xlab = "x1", ylab="",
     main = "Latin Hypercube Sampling")
spacings <- seq(0, 1, length = nn + 1)
for(i in 2:(nn )){
  lines(x = c(0, 1), y = rep(spacings[i],2), lty = 2)
  lines(x = rep(spacings[i],2), y = c(0,1), lty = 2)
}

lhmm.sample <- lhs::maximinLHS(nn, 2)
plot(lhmm.sample)

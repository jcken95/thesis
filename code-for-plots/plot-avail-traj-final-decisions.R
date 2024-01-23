##  plot the availability of the 6 decisions
source("~/optim-main/17dim/wave2-dat/Rfiles/utility-fn.R")
dat <- list()
for (i in 1:12) {
  
  location <- paste0("~/optim-main/wave2-dat/matlab-dat/final-decisions3/OutputOuter",i,".mat")
  y <- R.matlab::readMat(location)$y
  
  tmp <- matrix(ncol = 240, nrow  = 300)
  for (j in 1:300) {
    tmp[j, ] <- as.vector(y[[j]][[1]])
  }
  dat[[i]] <- tmp
}
#for(i in 1:12) dat[[i]] <- dat[[i]][1:30,]
par(mfrow = c(3, 2))

Time <- seq(0,5,length=240)
par(mfrow = c(4, 3))
for(i in 1:12) {
  quants <- apply(dat[[i]],  2, quantile, probs  = c(0.05, 0.2, 0.5, 0.8, 0.95))
  the.title <- paste0("Decision ", i)
  plot(0,1, ylim= c(min(unlist(dat)),1), xlim  = c(0, 5), type = "n",
       xlab = "Time/years", ylab  = "Availability",
       main = the.title)
  for(j in 1:300) {
    lines(Time, dat[[i]][j,], col = rgb(0,0,0, alpha = 0.1))
  }
  lines(Time, quants[3,], col = 2, lwd = 2.5)
  lines(Time, quants[1,], lty = 3, col = "red", lwd = 2.5)
  lines(Time, quants[5,], lty = 3, col = "red", lwd = 2.5)
  lines(Time, quants[2,], lty = 2, col = "red", lwd = 2.5)
  lines(Time, quants[4,], lty = 2, col = "red", lwd = 2.5)
}

u.avail.ts <- list()
for(i in 1:12) u.avail.ts[[i]] <- u.avail(dat[[i]], 0.95, 50)
par(mfrow = c(3,4))
for(i in 1:12) {
  quants <- apply(u.avail.ts[[i]],  2, quantile, probs  = c(0.05, 0.2, 0.5, 0.8, 0.95))
  the.title <- paste0("Decision ", i)
  plot(0,1, ylim= c(0,1), xlim  = c(0, 5), type = "n",
       xlab = "Time/years", ylab  = "Utility",
       main = the.title)
  for(j in 1:300) {
    lines(Time, u.avail.ts[[i]][j,], col = rgb(0,0,0, alpha = 0.1))
  }
  lines(Time, quants[3,], col = 2, lwd = 1.5)
  lines(Time, quants[1,], lty = 3, col  = "red", lwd = 1.5)
  lines(Time, quants[5,], lty = 3, col  = "red", lwd = 1.5)
  lines(Time, quants[2,], lty = 2, col  = "red", lwd = 1.5)
  lines(Time, quants[4,], lty = 2, col  = "red", lwd = 1.5)
}
X <- as.matrix(read.table("~/optim-main/wave2-dat/sensible-decisions-nomean3.csv", sep = ","))
limits <- readRDS("~/optim-main/wave2-dat/Rfiles/limits.RDS")
u.ts <- list()
## below gives the mean utility
u.all <- function(A.ts, x, weights, capacity, capacity.range, pc.range){
  u0  <- rep(0,3)
  u0[1] <- mean( u.avail(A.ts, 0.95,50))
  u0[2] <- u.warehouse(capacity, capacity.range )
  u0[3] <- mean(1 - ((x[10:18]-pc.range[1])/abs(diff(pc.range)) )^2)
  (u0 * weights)
}
pc.range <- range(limits$critpc)
for(i in 1:12) {
  u.ts[[i]] <- 0.8*u.avail.ts[[i]] + 
    0.1*u.warehouse(sum(X[i, 1:9]), range(limits$capacity)) + 
    0.1*mean(1 - ((X[i, 10:18]-pc.range[1])/abs(diff(pc.range)) )^2)
}
##  can I get a std err?
bestres <- readRDS("~/optim-main/wave2-dat/bestres2.RDS")

u.means <- unlist(lapply(u.ts, mean))
u.stderr <- unlist(lapply(lapply(u.ts, rowMeans), sd))/sqrt(300)
Y <- matrix(nrow = 3, ncol = length(u.means))
Y[1, ] <- u.means
Y[2, ] <- u.stderr
Y[3, ] <- ( (bestres$m - Y[1, ])/sqrt(bestres$v + (Y[2, ])^2)   )
scales::scientific(Y)
hist(u.ts[[1]])
## check mean is unimodal ...
par(mfrow = c(2,3))
N <- length(u1)
nn <- 300
for(i in 1:6) {
  u.bootstrap <- vector(length=nn)
  u1 <- rowMeans(u.ts[[i]])
  for(j in 1:nn) {
    samp <- sample(u1, size = 30, replace = TRUE)
    u.bootstrap[j] <- mean(samp)
  }
  hist(u.bootstrap)
}
# verify maximum is plausible  ...
(u.means[3]  - 0.8314)/sqrt(8.749*1e-5 + u.stderr[3]^2)


u.means[3] + c(-3,3)*u.stderr[3]
u.stderr^2

## also check bootstrap
set.seed(6789)
d <- readRDS("dat1-nomean-wave2.RDS")$U
subset <- sample(1:nrow(d), 4, replace = FALSE)
b <- 1000
bsize <- 30
bootstrap.res <- matrix(ncol = b, nrow = length(subset))
for(i in 1:nrow(bootstrap.res)) {
  x <- d[subset[i],]
  for(j in 1:b) {
    xreplace <- sample(x, bsize, replace = TRUE)
    bootstrap.res[i,j] <- mean(xreplace)
  }
}
par(mfrow = c(2,2))
for(i in 1:4)  hist(bootstrap.res[i,], main = "",
                    xlab = paste0("Draws of y(x",i,")"))
mtext("Bootstrap distributions of y(x)", side = 3, line = -2, outer = TRUE, cex=1.5)
for(i in 1:4) {qqnorm(scale(bootstrap.res[i,]));abline(0,1)}


par(mfrow = c(1,1))
i <- 1
quants <- apply(dat[[i]],  2, quantile, probs  = c(0.05, 0.2, 0.5, 0.8, 0.95))
the.title <- "Availability trajectories from Athena"
plot(0,1, ylim= c(min(unlist(dat)),1), xlim  = c(0, 5), type = "n",
     xlab = "Time/years", ylab  = "Availability",
     main = the.title)
for(j in 1:300) {
  lines(Time, dat[[i]][j,], col = rgb(0,0,0, alpha = 0.3))
}
lines(Time, quants[3,], col = "red", lwd = 3)
lines(Time, quants[1,], lty = 3, col = "red", lwd = 3)
lines(Time, quants[5,], lty = 3, col = "red", lwd = 3)
lines(Time, quants[2,], lty = 2, col = "red", lwd = 3)
lines(Time, quants[4,], lty = 2, col = "red", lwd = 3)

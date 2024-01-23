## compare designs ...
set.seed(987)
d <- readRDS("~/optim-main/wave2-dat/design/2-1design.RDS")
D1 <- readRDS("~/optim-main/wave2-dat/design/2-1alm-nomean.RDS")
par(mfrow = c(3,6))
D11 = matrix(nrow = nrow(D1), ncol = 1 + ncol(D1))
D11[,1] <- 1 - rowSums(D1[,1:8])
D11[, -1] <- D1
D11.rand <- matrix(nrow = 1000, ncol = 1 + ncol(d$design))
D11.rand[, -1] <- d$design[sample(1:nrow(D11.rand), 1000, replace = FALSE), ]
D11.rand[,1] <- 1 - rowSums(D11.rand[, 2:9])
for(i in 1:18) {
  hist(D11[,i], prob = T)
  hist(D11.rand[,i], prob = T, col = rgb(1, 0, 0, alpha = 0.3), add = T)
  abline(v = range(D11[,i]))
  abline(v = range(D11.rand[,i]), col = 2, lty = 2)
}
## widths of designs
widths1 <- matrix(ncol = 18, nrow = 2)
for(i in 1:18) {
  widths1[1, i] <- abs(diff(range(D11[,i])))
  widths1[2, i] <- abs(diff(range(D11.rand[,i])))
}
log.gmean1 <- mean(log(widths1[1,-1]) - log(widths1[2,-1]))
exp(log.gmean1)
## compare designs ...
d <- readRDS("~/optim-main/wave2-dat/design/2-2design.RDS")
D2 <- readRDS("~/optim-main/wave2-dat/design/2-2alm-nomean.RDS")
par(mfrow = c(3,6))
D22 = matrix(nrow = nrow(D2), ncol = 1 + ncol(D2))
D22[,1] <- 1 - rowSums(D2[,2:8])
D22[, -1] <- D2
D22.rand <- matrix(nrow = 1000, ncol = 1 + ncol(d$design))
D22.rand[, -1] <- d$design[sample(1:nrow(D22.rand), 1000, replace = FALSE), ]
D22.rand[,1] <- 1 - rowSums(D22.rand[, 2:9])
for(i in 1:18) {
  hist(D22[,i], prob = T)
  hist(D22.rand[,i], prob = T, col = rgb(1, 0, 0, alpha = 0.3), add = T)
  abline(v = range(D22[,i]))
  abline(v = range(D22.rand[,i]), col = 2, lty = 2)
}
## widths
widths2 <- matrix(ncol = 18, nrow = 2)
for(i in 1:18) {
  widths2[1, i] <- abs(diff(range(D22[,i])))
  widths2[2, i] <- abs(diff(range(D22.rand[,i])))
}
log.gmean2 <- mean(log(widths2[1,-1]) - log(widths2[2,-1]))
exp(log.gmean2)
## calculate minimum and maximum distances

dist.alm1 <- dist(D11)
dist.alm2 <- dist(D22)
dist.rand1 <- dist(D11.rand)
dist.rand2 <- dist(D22.rand)
max.distances <- min.distances <- matrix(ncol = 2, nrow = 2)
max.distances[1,1] <- max(dist.alm1)
max.distances[1,2] <- max(dist.alm2)
max.distances[2,1] <- max(dist.rand1)
max.distances[2,2] <- max(dist.rand2)

min.distances[1,1] <- min(dist.alm1)
min.distances[1,2] <- min(dist.alm2)
min.distances[2,1] <- min(dist.rand1)
min.distances[2,2] <- min(dist.rand2)

max.distances
min.distances

##  plot random and ALM designs ...
set.seed(654)
alm1 <- readRDS("~/optim-main/wave2-dat/design/2-1alm-nomean.RDS")
rand1 <- readRDS("~/optim-main/wave2-dat/design/2-1design.RDS")
dim(alm1)
dim(rand1$design)
D1 <- matrix(ncol = 18, nrow = 1000)
D2 <- D1
D1[,-1] <- alm1[,1:17]
D1[,1] <- 1 - rowSums(D1[,2:9])
indices <- sample(1:nrow(rand1$design), size = nrow(D1), replace = FALSE)
D2 <- D1
D2[,-1] <- rand1$design[indices, 1:17]
D2[, 1] <- 1 - rowSums(D2[,2:9])
par(mfrow = c(3,6))
ylims = matrix(nrow = 18, ncol = 2)
ylims[,1] <- 0
ylims[1,2] <- 15
ylims[2,2] <- 9
ylims[3,2] <- 10
ylims[4,2] <- 10
ylims[5,2] <- 9
ylims[6,2] <- 10
ylims[7,2] <- 10
ylims[8,2] <- 9
ylims[9,2] <- 7
ylims[10:18,2] <- 2
for(i in 1:18) {
  hist(D1[,i], prob = TRUE, main = "",
       xlab = paste0("x",i), ylim = ylims[i,],
       col = rgb(0, 0.5,0.75, alpha = 0.8))
  abline(v = range(D1[,i]), col = rgb(0, 0.5,0.75, alpha = 0.8))
  hist(D2[,i], col  = rgb(1, 0, 0, alpha = 0.4), add  = TRUE, prob = TRUE)
  abline(v = range(D2[,i]), col= 2, lty = 2)
}

## ranges of each simplex part ...
ranges <- matrix(ncol = 9, nrow = 2)
for(i in 1:9) {
  ranges[1, i] <- diff(range(D1[,i]))
  ranges[2, i] <- diff(range(D2[,i]))
}
100 * (ranges[2,] - ranges[1,])/ranges[2,]
summary(100 * (ranges[2,] - ranges[1,])/ranges[2,])
### now repeat for W2
##  plot random and ALM designs ...
rm(alm1, rand1)
alm2 <- readRDS("~/optim-main/wave2-dat/design/2-2alm-nomean.RDS")
rand2 <- readRDS("~/optim-main/wave2-dat/design/2-2design.RDS")
dim(alm2)
dim(rand2$design)
D1 <- matrix(ncol = 18, nrow = 1000)
D2 <- D1
D1[,-1] <- alm2[,1:17]
D1[,1] <- 1 - rowSums(D1[,2:9])
indices <- sample(1:nrow(rand2$design), size = nrow(D1), replace = FALSE)
D2 <- D1
D2[,-1] <- rand2$design[indices, 1:17]
D2[, 1] <- 1 - rowSums(D2[,2:9])
par(mfrow = c(3,6))
ylims = matrix(nrow = 18, ncol = 2)
ylims[,1] <- 0
ylims[1,2] <- 7
ylims[2,2] <- 10
ylims[3,2] <- 7
ylims[4,2] <- 8
ylims[5,2] <- 11
ylims[6,2] <- 12
ylims[7,2] <- 5
ylims[8,2] <- 12
ylims[9,2] <- 17
ylims[10,2] <- 5
ylims[11,2] <- 4
ylims[12,2] <- 5
ylims[13,2] <- 6
ylims[14,2] <- 5
ylims[15,2] <- 8
ylims[16,2] <- 4
ylims[17,2] <- 5
ylims[18,2] <- 7
for(i in 1:18) {
  hist(D1[,i], prob = TRUE, main = "",
       xlab = paste0("x",i), ylim = ylims[i,],
       col = rgb(0, 0.5,0.75, alpha = 0.8))
  abline(v = range(D1[,i]), col = rgb(0, 0.5,0.75, alpha = 0.8))
  hist(D2[,i], col  = rgb(1, 0, 0, alpha = 0.4), add  = TRUE, prob = TRUE)
  abline(v = range(D2[,i]), col= 2, lty = 2)
}

## ranges of each simplex part ...
ranges <- matrix(ncol = 18, nrow = 2)
for(i in 1:18) {
  ranges[1, i] <- diff(range(D1[,i]))
  ranges[2, i] <- diff(range(D2[,i]))
}
mean(100 * (ranges[2,] - ranges[1,])/ranges[2,])

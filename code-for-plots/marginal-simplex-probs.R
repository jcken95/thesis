## beta plot
k = seq(3:15)
values = c(0.2, 0.4, 0.6, 0.8)
P = matrix(ncol = length(values), nrow  = length(k))
for(i in 1:length(k)) {
 P[i, ] = pbeta(values, 1,  k[i])
}
plot(k, 1 - P[,1], type  = "l", log = "y", ylim = c(min(1-P), 0.7),
     main = "P(Xi > b) for choices of b and k", ylab = "P(Xi > b)",
     xaxt = "n")
for(i in 2:length(values)) {
  lines(k, 1 - P[, i], lty = i, log  = "y")
}
legend("bottomleft", lty = 1:4, legend = values, title  = "b", bty = "n")
axis(side = "1", at = k)
abline(v = 8, lty = 2, col  =2)

set.seed(8263)
time <- exp(rnorm(20, mean  = -1, sd = 0.5))
money <- exp(rnorm(20,  mean = -1, sd = 0.5))
d = data.frame(time  = time, money = money)
pfront <- rPref::psel(d, rPref::low(time)*rPref::low(money))
order <- sort(pfront$time, index.return  = TRUE)$ix
pfront_index = as.numeric(rownames(pfront))
pfront <- pfront[order, ]
plot(time[-c(2, pfront_index)], money[-c(2, pfront_index)],
     main = "Illustrative Pareto Front",
     xlab = "Time", ylab  = "Money",
     ylim = range(money), xlim = range(time),
     pch = 4)
lines(pfront,  col = "red")
points(pfront, col = "red", pch = 19)
legend("topleft", pch = c(4, 19), col = c("black", "red"),
       legend = c("Pareto Dominated", "Pareto Front"), bty = "n")

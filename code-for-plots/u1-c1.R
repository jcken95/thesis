source("~/optim-main/17dim/wave2-dat/Rfiles/utility-fn.R")
x <- seq(0, 1, length = 100)
u <- u.avail(x, 0.95, 50)
plot(x, u, type = "l",
     xlab = "c1 / Availabilty",
     ylab = "u1(c1)",
     main = "Point-wise utility function for c1")

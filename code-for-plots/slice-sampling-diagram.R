## slice sampling graph
set.seed(22722)##22nd June (20)22
phi = function(x) {
  
  dnorm(x, 0, 1) + 0.3*dnorm(x, -2, 0.3) + 0.7*dnorm(x, 1.5, 1.5)
  
}
xx = seq(-5, 7, length = 200)
plot(xx, phi(xx), type =  "l", 
     main = "Univariate Slice Sampling Illustration",
     xlab = "x", ylab  = "phi(x)")
  
x0  = runif(1, -2, 2)
f0 = phi(x0)
abline(h = f0, col = "blue")
y0 = runif(1, 0, f0)
abline(h  = y0, col = "red")
phi2  = function(x, phi0) {phi(x) - phi0}
x_star  = uniroot(phi2, lower  = 0, upper = 2, phi0 = y0)$root[[1]]
## solve for the intersection points
x = rep(NA, 4)

x[1] = uniroot(phi2, lower = -4, upper = -2, phi0 = y0)$root[[1]]
x[2] = uniroot(phi2, lower = -2, upper = -1.5, phi0 = y0)$root[[1]]
x[3]  = uniroot(phi2, lower = -1, upper  = 0, phi0 = y0)$root[[1]]
x[4]  = uniroot(phi2, lower = 0, upper  = 5, phi0 = y0)$root[[1]]
for(i in 1:4){
  segments(x[i],  0, x[i], phi(x[i]),  col  = "red", lty  = 2)
}
segments(x[1],  0, x[2], 0,  col  = "red", lty  = 2)
segments(x[3],  0, x[4], 0,  col  = "red", lty  = 2)
segments(x0, 0, x0, phi(x0), col  = "blue")
legend("topright",
       col = c("blue", "red", "red", "black"),
       lty  = c(1, 1, 2, 1),
       legend = c("x0 and phi(x0)",
                  "y0",
                  "sample space for x1",
                  "phi(x)"))

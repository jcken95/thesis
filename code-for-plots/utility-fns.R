u.avail <- function(x, a, b){
  u.min <- -a/sqrt(1+a*a*b)
  u.max <- (1-a)/sqrt( 1 + b*(1-a)^2 )
  res <- (x-a)/sqrt(1+b*(x-a)^2)
  (res - u.min)/(u.max - u.min)
}
u0 = Vectorize(u.avail, vectorize.args = "x")
xx = seq(from = 0, to  = 1, length = 100)
plot(xx, (u0(xx, 0.8, 50)), type = "l")
u0(0.9, 0.95, 50)
u0(0.95, 0.95, 50)
a = 0.95; b = 50
u.min <- -a/sqrt(1+a*a*b)
u.max <- (1-a)/sqrt( 1 + b*(1-a)^2 )
u.max
u.min

plot(xx, 1 - (1-xx)^2, type = "l")
1 - ((75-50)/50)^2

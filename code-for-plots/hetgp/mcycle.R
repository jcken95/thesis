par(mfrow=c(1,1))
plot(MASS::mcycle, ylim = c(-170,100), xlab = "Time", Ylab = "Acceleration", main = "HomGP for Motorcycle Data")
gpmodel <- hetGP::mleHomGP(X = MASS::mcycle$times, Z = MASS::mcycle$accel)
x0 <- seq(from = 0, to = 100, length = 100)
preds <- predict(gpmodel, x = matrix(x0, ncol=1))  
tot.var <- preds$sd2 + preds$nugs
lwr <- preds$mean + 1.96*sqrt(tot.var)
upr <- preds$mean - 1.96*sqrt(tot.var)
polygon(c(x0, rev(x0)), c(lwr, rev(upr)), col = rgb(1,0,0,alpha=0.3), border = NA)
lines(x0, preds$mean, lwd=2)
points(MASS::mcycle, pch=20)
legend("bottomleft", legend = c("mean", "m +/- 1.96sd"),
       col =  c(1, rgb(1,0,0,alpha = 0.3)), bty="n", lty=c(1,1), lwd=2)
## fit a HetGP to the same data set ...
library(MASS)
X <- matrix(mcycle$times, ncol = 1)
Z <- mcycle$accel
nvar <- 1
plot(X, Z, ylim = c(-170,100), ylab = "Acceleration", xlab = "Time",  main = "HetGP for Motorcycle Data")


## Model fitting
model <- hetGP::mleHetGP(X = X, Z = Z, lower = rep(0.1, nvar), upper = rep(50, nvar),
                  covtype = "Matern5_2")

## Display averaged observations
points(mcycle, pch = 20)

## A quick view of the fit                  
summary(model)

## Create a prediction grid and obtain predictions
xgrid <- matrix(seq(0, 60, length.out = 301), ncol = 1) 
predictions <- predict(x = xgrid, object =  model)

## Display mean predictive surface
#lines(xgrid, predictions$mean, col = 'red', lwd = 2)
upr <- predictions$mean + 1.96*sqrt(predictions$sd2 + predictions$nugs)
lwr <- predictions$mean - 1.96*sqrt(predictions$sd2 + predictions$nugs)

polygon(c(xgrid, rev(xgrid)), c(lwr, rev(upr)), col = rgb(1,0,0,alpha=0.3), border = NA)
lines(xgrid, predictions$mean, lwd = 2)
legend("bottomleft", legend = c("mean", "m +/- 1.96sd"),
       col =  c(1, rgb(1,0,0,alpha = 0.3)), bty="n", lty=c(1,1), lwd=2)

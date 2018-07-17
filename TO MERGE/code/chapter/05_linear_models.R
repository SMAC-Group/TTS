## @knitr modelSelectionEg
# Load libraries
library(astsa)

# Load data
data(gtemp)

# Degree of polynomial regression
deg_max = 30

# Construct design matrix (no intercept)
year = time(gtemp)
X = cbind(year)
for (i in 2:deg_max){X = cbind(X,year^i)}

# Define response vector
y = gtemp

# Initialisation
model.AIC = rep(NA,deg_max)
model.BIC = rep(NA,deg_max)
model.pred = matrix(NA,deg_max,length(y))

# Fit models
for (i in 1:deg_max){
  # Fit model
  model = lm(y~X[,1:i])
  
  # Compute AIC, BIC and \hat{y}
  model.AIC[i] = AIC(model)
  model.BIC[i] = BIC(model)
  model.pred[i,] = fitted(model)
}

# Compute best AIC and BIC
aic.best = which.min(model.AIC)
bic.best = which.min(model.BIC)

# Plot results
par(mfrow = c(1,2))
plot(NA, xlim = c(1,deg_max),
     ylim = range(cbind(model.AIC, model.BIC)), 
     xlab = "Polynomial order", ylab = "AIC/BIC")
grid()
lines(model.AIC, type = "b", col = "dodgerblue3", lty = 2)
lines(model.BIC, type = "b", col = "darkgoldenrod2", pch = 22)
points(aic.best,model.AIC[aic.best], col = "dodgerblue3", 
       pch = 16, cex = 2)
points(bic.best,model.BIC[bic.best], col = "darkgoldenrod2", 
       pch = 15, cex = 2)

legend("topright", c("BIC","Min BIC","AIC","Min AIC"),
pch = c(22,15,21,16), pt.cex = rep(c(1,2),2),
col = rep(c("darkgoldenrod2","dodgerblue3"), each = 2),
lty = c(1,NA,2,NA), lwd = c(1,NA,1,NA),
bty = "n", bg = "white", box.col = "white", cex = 1.2)

plot(NA, xlim = range(year), ylim = range(y), 
     xlab = "Time (year)", 
     ylab = "Global Temperature Deviation")
grid()
lines(gtemp, col = "darkgrey")
lines(cbind(year,model.pred[aic.best,])[,2], 
      col = "dodgerblue3", lty = 2, lwd = 2)

legend("topleft", c("Data","Fitted (best AIC)"),
       col = c("darkgrey","dodgerblue3"),
       lty = c(1,2), lwd = c(1,2),
       bty = "n", bg = "white", box.col = "white", cex = 1.2)



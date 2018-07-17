## @knitr causalAR2
plot(NA, xlim = c(-2.1,2.1), ylim = c(-1.1,1.1), xlab = expression(phi[1]),
     ylab = expression(phi[2]), cex.lab = 1.5)
grid()

# Adding boundary of constraint |phi_1| < 2
abline(v = c(-2,2), lty = 2, col = "darkgrey")

# Adding boundary of constraint |phi_2| < 1
abline(h = c(-1,1), lty = 2, col = "darkgrey")

# Adding boundary of constraint phi_2 = 1 - phi_1 
phi1 = seq(from = -2, to = 2, length.out = 10^3)
phi2.c1 = 1 - phi1
lines(phi1, phi2.c1, lty = 2, col = "darkgrey")

# Adding boundary of constraint phi_2 = 1 + phi_1 
phi1 = seq(from = -2, to = 2, length.out = 10^3)
phi2.c2 = 1 + phi1
lines(phi1, phi2.c2, lty = 2, col = "darkgrey")

# Adding admissible region
polygon(c(phi1,rev(phi1)),c(rep(-1,10^3),
        rev(phi2.c1[501:1000]),rev(phi2.c2[1:500])),
        border = NA, col= rgb(0,0,1, alpha = 0.1))

# Adding text
text(0,-0.5, c("Causal Region"))

## @knitr ACFAR2cont
library(exts)
autoplot(theo_acf(AR(phi = c(1.3, -0.4))))

## @knitr causalAR2part2
plot(NA, xlim = c(-2.1,2.1), ylim = c(-1.1,1.1), xlab = expression(phi[1]),
     ylab = expression(phi[2]), cex.lab = 1.5)
grid()

# Adding boundary of constraint |phi_1| < 2
abline(v = c(-2,2), lty = 2, col = "darkgrey")

# Adding boundary of constraint |phi_2| < 1
abline(h = c(-1,1), lty = 2, col = "darkgrey")

# Adding boundary of constraint phi_2 = 1 - phi_1 
phi1 = seq(from = -2, to = 2, length.out = 10^3)
phi2.c1 = 1 - phi1
lines(phi1, phi2.c1, lty = 2, col = "darkgrey")

# Adding boundary of constraint phi_2 = 1 + phi_1 
phi1 = seq(from = -2, to = 2, length.out = 10^3)
phi2.c2 = 1 + phi1
lines(phi1, phi2.c2, lty = 2, col = "darkgrey")

# AR2 with a single root
phi2.s = -phi1^2/4
lines(phi1, phi2.s, lty = 1, col = "darkgrey")

# AR2 with complex roots
polygon(c(phi1,rev(phi1)),c(rep(-1,10^3),rev(phi2.s)),
        border = NA, col= rgb(1,0,0, alpha = 0.1))
text(0,-0.5, c("Complex roots"))

# AR2 with real roots
polygon(c(phi1,rev(phi1)),c(phi2.s, rev(phi2.c1[501:1000]),rev(phi2.c2[1:500])),
        border = NA, col= rgb(0,0,1, alpha = 0.1))
text(0,0.25, c("Real roots"))

# Adding models
points(1,-1/4, pch = 16)
points(0.5, 0.25, pch = 16)
points(-1.5, -0.75, pch = 16)

text(1,-0.32, c("Model 1"), cex = 0.75)
text(0.5, 0.18, c("Model 2"), cex = 0.75)
text(-1.5, -0.82, c("Model 3"), cex = 0.75)

## @knitr ACFAR2eg
library("exts")
library("gridExtra")

# Define models
m1 = AR(phi = c(1, -0.25))
m2 = AR(phi = c(0.5,0.25))
m3 = AR(phi = c(-1.5, -0.75))

# Theoretical ACF
acf1 = theo_acf(m1)
acf2 = theo_acf(m2)
acf3 = theo_acf(m3)

# Plot ACFs
a1 = autoplot(acf1)
a2 = autoplot(acf2)
a3 = autoplot(acf3)
grid.arrange(a1, a2, a3, nrow = 1)


## @knitr ACF_ARpeg
library(exts)
library(gridExtra)

# Define models
m1 = AR(phi = c(0.3, -0.4, 0.3, 0.1))
m2 = AR(phi = c(1, -1/4, -0.25))
m3 = AR(phi = c(1/2, 0.1, -1/2, -0.3, 0.3))

# Theoretical ACF
acf1 = theo_acf(m1)
acf2 = theo_acf(m2)
acf3 = theo_acf(m3)

# Plot ACFs
a1 = autoplot(acf1)
a2 = autoplot(acf2)
a3 = autoplot(acf3)
grid.arrange(a1, a2, a3, nrow = 1)

## @knitr asy_conf_int
set.seed(2)
x = arima.sim(n = 50, list(ar = 0.99, ma = 0, sd = 1))
fit = arima(x, order = c(1,0,0), include.mean = FALSE)
c(fit$coef - 1.96*sqrt(fit$var.coef),fit$coef + 1.96*sqrt(fit$var.coef))

## @knitr boot_conf_int
set.seed(2)
x = arima.sim(n = 50, list(ar = 0.99, ma = 0, sd = 1))
fit = arima(x, order = c(1,0,0), include.mean = FALSE)

B = 500
est.phi = rep(NA,B)

for(i in 1:B){
  
  set.seed(i+2)
  
  x.star = arima.sim(n = 50, list(ar = fit$coef, ma = 0, sd = fit$sigma2))
  est.phi[i] = try(arima(x.star, order = c(1,0,0), include.mean = FALSE)$coef, silent = T)
  
}

suppressWarnings(quantile(as.numeric(est.phi),probs=c(0.025,0.975),na.rm = T))

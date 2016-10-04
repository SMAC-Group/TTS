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

## @knitr estimXbar

# Number of Monte-Carlo replications
for (i in 1:B){
  # Simulate AR(1)
  Xt = gen.
}





## @knitr admissibility
plot(NA, xlim = c(-1.1,1.1), ylim = c(-1.1,1.1), xlab = expression(rho[1]),
     ylab = expression(rho[2]), cex.lab = 1.5)
grid()

# Adding boundary of constraint |rho_1| < 1
abline(v = c(-1,1), lty = 2, col = "darkgrey")

# Adding boundary of constraint |rho_2| < 1
abline(h = c(-1,1), lty = 2, col = "darkgrey")

# Adding boundary of non-linear constraint
rho1 = seq(from = -1, to = 1, length.out = 10^3)
rho2 = (rho1^2 - 1) + rho1^2 
lines(rho1, rho2, lty = 2, col = "darkgrey")

# Adding admissible region
polygon(c(rho1,rev(rho1)),c(rho2,rep(1,10^3)),
        border = NA, col= rgb(0,0,1, alpha = 0.1))

# Adding text
text(0,0, c("Admissible Region"))


## @knitr basicACF

# Simulate iid gaussian RV (i.e. white noise)
Xt = rnorm(100)

# Compute autocorrelation
acf_Xt = acf(Xt)

# Plot autocorrelation
plot(acf_Xt)

## @knitr basicACF2

# Plot autocorrelation with confidence bands
plot(acf_Xt)

## @knitr simulationACF

# Number of Monte Carlo replications
B = 10000

# Define considered lag
h = 3

# Sample size considered
T = c(5,10,30,300)

# Initialisation
result = matrix(NA,B,length(T))

# Set seed
set.seed(1)

# Start Monte Carlo
for (i in 1:B){
  for (j in 1:length(T)){
    # Simluate process
    Xt = rnorm(T[j])
    
    # Save autocorrelation at lag h
    result[i,j] = acf(Xt, plot = FALSE)$acf[h+1]
  }
}

# Plot results
par(mfrow = c(1,length(T)))
for (i in 1:length(T)){
  # Estimated empirical distribution
  hist(result[,i], col = "lightgrey", main = paste("Sample size T =",T[i]), probability = TRUE, xlim = c(-1,1), xlab = " ")
  
  # Asymptotic distribution
  xx = seq(from = -10, to = 10, length.out = 10^3)
  yy = dnorm(xx,0,1/sqrt(T[i]))
  lines(xx,yy, col = "red")
}

## @knitr RW
# In this example, we simulate a large number of random walks
# Number of simulated processes
B = 200

# Length of random walks
n = 1000

# Output matrix
out = matrix(NA,B,n)

for (i in 1:B){
  # Simulate random walk
  Xt = cumsum(rnorm(n))
  
  # Store process
  out[i,] = Xt
}

# Plot random walks
plot(NA, xlim = c(1,n), ylim = range(out), xlab = "Time", ylab = " ")
color = sample(topo.colors(B, alpha = 0.5))
for (i in 1:B){
  lines(out[i,], col = color[i])
}

# Add 95% confidence region
lines(1:n, 1.96*sqrt(1:n), col = 2, lwd = 2, lty = 2)
lines(1:n, -1.96*sqrt(1:n), col = 2, lwd = 2, lty = 2)

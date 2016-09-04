## @knitr GSPC
# Load package
library(quantmod)

# Download S&P index
getSymbols("^GSPC", from="1990-01-01", to = Sys.Date())

# Compute returns
GSPC.ret = ClCl(GSPC)

# Plot index level and returns
par(mfrow = c(1,2))
plot(GSPC, main = " ", ylab = "Index level")
plot(GSPC.ret, main = " ", ylab = "Daily returns")

## @knitr GSPCacf
sp500 = na.omit(GSPC.ret)
names(sp500) = paste("S&P 500 (1990-01-01 - ",Sys.Date(),")", sep = "")
plot(ACF(sp500))

## @knitr XtYt
library(gmwm)
library(gridExtra)

# Simulate Xt
set.seed(1)
model = AR1(phi = 0.5, sigma2 = 1)
Xt = gen.gts(model)

# Construct Yt
epsilon = 0.01
nb_outlier = rbinom(1,length(Xt),epsilon)
Yt = Xt
Yt[sample(1:length(Xt),nb_outlier)] = rnorm(nb_outlier,0,10)

# Add names
Xt = gts(Xt)
Yt = gts(Yt, name = paste("(",expression(Y[t]),")",sep = ""))

# Plot data
a = autoplot(Xt) + ylim(range(Yt)) + ylab("(Xt)")
b = autoplot(Yt) + ylab("(Yt)")
grid.arrange(a, b, nrow = 2)


## @knitr GSPCracf
# Construct gts objects
sp500c = gts(sp500, name = 'Non-robust Estimator')
sp500r = gts(sp500, name = 'Robust Estimator')

# Plot data
a = plot(ACF(sp500c))
inter = ACF(sp500r)
inter[,,] = robacf(sp500r, plot=FALSE)$acf
b = plot(inter)
grid.arrange(a, b, nrow = 1)

## @knitr hydro_ACF

# Load packages
library(gmwm)
library(gridExtra)
library(robcor)

# Load data
data("hydro", package = "datapkg")

# Construct gts objects
hydro1 = gts(hydro, name = 'Non-robust Estimator')
hydro2 = gts(hydro, name = 'Robust Estimator')

# Plot data
a = plot(ACF(hydro1))
inter = ACF(hydro2)
inter[,,] = robacf(hydro2, plot=FALSE)$acf
b = plot(inter)
grid.arrange(a, b, nrow = 1)

## @knitr simulationRobust

# Define sample size
n = 100

# Define proportion of "extreme" observation
alpha = 0.05

# Extreme observation are generated from N(0,sigma2.cont)
sigma2.cont = 10

# Number of Monte-Carlo replications
B = 1000

# Define model AR(1)
phi = 0.5
sigma2 = 1
model = AR1(phi = phi, sigma2 = sigma2)

# Initialization of result array
result = array(NA, c(B,2,20))

# Set seed for reproducibility
set.seed(3298)

# Start Monte-Carlo
for (i in seq_len(B)){
  # Simulate AR(1)
  Xt = gen.gts(model, N = n)
  
  # Create a copy of Xt
  Yt = Xt
  
  # Add a proportion alpha of extreme observations to Yt
  Yt[sample(1:n,round(alpha*n))] = rnorm(round(alpha*n), 0, sigma2.cont)
  
  # Compute ACF of Xt and Yt
  acf_Xt = ACF(Xt)
  acf_Yt = ACF(Yt)
  
  # Store ACFs
  result[i,1,] = acf_Xt[1:20]
  result[i,2,] = acf_Yt[1:20]
}


# Compare empirical distribution of ACF based on Xt and Yt

# Vector of lags considered (h <= 20)
lags = c(1,2,5,10) + 1

# Make graph
par(mfrow = c(2,2))

for (i in seq_along(lags)){
  boxplot(result[,1,lags[i]], result[,2,lags[i]], col = "lightgrey",
          names = c("Uncont.","Cont."), main = paste("lag: h = ", lags[i]-1),
          ylab = "Sample autocorrelation")
  abline(h = phi^(lags[i]-1), col = 2, lwd = 2)
}

## @knitr simulationRobust2

# Load packages
library("robcor")

# Define sample size
n = 100

# Define proportion of "extreme" observation
alpha = 0.05

# Extreme observation are generated from N(0,sigma2.cont)
sigma2.cont = 10

# Number of Monte-Carlo replications
B = 1000

# Define model AR(1)
phi = 0.5
sigma2 = 1
model = AR1(phi = phi, sigma2 = sigma2)

# Initialization of result array
result = array(NA, c(B,2,20))

# Set seed for reproducibility
set.seed(5585)

# Start Monte-Carlo
for (i in seq_len(B)){
  # Simulate AR(1)
  Xt = gen.gts(model, N = n)
  
  # Add a proportion alpha of extreme observations to Yt
  Xt[sample(1:n,round(alpha*n))] = rnorm(round(alpha*n), 0, sigma2.cont)
  
  # Compute standard and robust ACF of Xt and Yt
  acf = ACF(Xt)
  rob_acf = robacf(Xt, plot=FALSE)$acf
  
  # Store ACFs
  result[i,1,] = acf[1:20]
  result[i,2,] = rob_acf[1:20]
}


# Compare empirical distribution of standard and robust ACF based on Xt

# Vector of lags considered (h <= 20)
lags = c(1,2,5,10) + 1

# Make graph
par(mfrow = c(2,2))

for (i in seq_along(lags)){
  boxplot(result[,1,lags[i]], result[,2,lags[i]], col = "lightgrey",
          names = c("Standard","Robust"), main = paste("lag: h = ", lags[i]-1),
          ylab = "Sample autocorrelation")
  abline(h = phi^(lags[i]-1), col = 2, lwd = 2)
}








## @knitr simulationRobust3

# Load packages
library("robcor")

# Define sample size
n = 100

# Define proportion of "extreme" observation
alpha = 0

# Extreme observation are generated from N(0,sigma2.cont)
sigma2.cont = 10

# Number of Monte-Carlo replications
B = 1000

# Define model AR(1)
phi = 0.5
sigma2 = 1
model = AR1(phi = phi, sigma2 = sigma2)

# Initialization of result array
result = array(NA, c(B,2,20))

# Set seed for reproducibility
set.seed(5585)

# Start Monte-Carlo
for (i in seq_len(B)){
  # Simulate AR(1)
  Xt = gen.gts(model, N = n)
  
  # Add a proportion alpha of extreme observations to Yt
  Xt[sample(1:n,round(alpha*n))] = rnorm(round(alpha*n), 0, sigma2.cont)
  
  # Compute standard and robust ACF of Xt and Yt
  acf = ACF(Xt)
  rob_acf = robacf(Xt, plot=FALSE)$acf
  
  # Store ACFs
  result[i,1,] = acf[1:20]
  result[i,2,] = rob_acf[1:20]
}


# Compare empirical distribution of standard and robust ACF based on Xt

# Vector of lags considered (h <= 20)
lags = c(1,2,5,10) + 1

# Make graph
par(mfrow = c(2,2))

for (i in seq_along(lags)){
  boxplot(result[,1,lags[i]], result[,2,lags[i]], col = "lightgrey",
          names = c("Standard","Robust"), main = paste("lag: h = ", lags[i]-1),
          ylab = "Sample autocorrelation")
  abline(h = phi^(lags[i]-1), col = 2, lwd = 2)
}






## @knitr estimXbar

# Define sample size
n = 10

# Number of Monte-Carlo replications
B = 5000

# Define grid of values for phi
phi = seq(from = 0.95, to = -0.95, length.out = 30)

# Define result matrix
result = matrix(NA,B,length(phi))

# Start simulation
for (i in seq_along(phi)){
  # Define model
  model = AR1(phi = phi[i], sigma2 = 1)
  
  # Monte-Carlo
  for (j in seq_len(B)){
    # Simulate AR(1)
    Xt = gen.gts(model, N = n)
    
    # Estimate Xbar
    result[j,i] = mean(Xt)
  }
}

# Estimate variance of Xbar
var.Xbar = apply(result,2,var)

# Compute theoretical variance
var.theo = (n - 2*phi - n*phi^2 + 2*phi^(n+1))/(n^2*(1-phi^2)*(1-phi)^2)

# Compute (approximate) variance
var.approx = 1/(n*(1-phi)^2)

# Compare variance estimations
plot(NA, xlim = c(-1,1), ylim = range(var.approx), log = "y", 
     ylab = expression(paste("var(", bar(X), ")")),
     xlab= expression(phi), cex.lab = 1)
grid()
lines(phi,var.theo, col = "deepskyblue4")
lines(phi, var.Xbar, col = "firebrick3")
lines(phi,var.approx, col = "springgreen4")
legend("topleft",c("Theoretical variance","Bootstrap variance","Approximate variance"), 
       col = c("deepskyblue4","firebrick3","springgreen4"), lty = 1,
       bty = "n",bg = "white", box.col = "white", cex = 1.2)

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
# Load package
library("gmwm")

# Set seed for reproducibility
set.seed(2241)

# Simulate 100 observation from a Gaussian white noise
Xt = gen.gts(WN(sigma2 = 1), N = 100)

# Compute autocorrelation
acf_Xt = ACF(Xt)

# Plot autocorrelation
plot(acf_Xt, show.ci = FALSE)

## @knitr basicACF2

# Plot autocorrelation with confidence bands 
plot(acf_Xt)

## @knitr simulationACF

# Number of Monte Carlo replications
B = 10000

# Define considered lag
h = 3

# Sample size considered
N = c(5,10,30,300)

# Initialisation
result = matrix(NA,B,length(N))

# Set seed
set.seed(1)

# Start Monte Carlo
for (i in seq_len(B)){
  for (j in seq_along(N)){
    # Simluate process
    Xt = rnorm(N[j])
    
    # Save autocorrelation at lag h
    result[i,j] = acf(Xt, plot = FALSE)$acf[h+1]
  }
}

# Plot results
par(mfrow = c(2,length(N)/2))
for (i in seq_along(N)){
  # Estimated empirical distribution
  hist(sqrt(N[i])*result[,i], col = "royalblue1", 
       main = paste("Sample size n =",N[i]), probability = TRUE,
       xlim = c(-4,4), xlab = " ")
  
  # Asymptotic distribution
  xx = seq(from = -10, to = 10, length.out = 10^3)
  yy = dnorm(xx,0,1)
  lines(xx,yy, col = "red", lwd = 2)
}

## @knitr RWsim
# In this example, we simulate a large number of random walks

# Number of simulated processes
B = 200

# Length of random walks
n = 1000

# Output matrix
out = matrix(NA,B,n)

# Set seed for reproducibility
set.seed(6182)

# Simulate Data
for (i in seq_len(B)){
  # Simulate random walk
  Xt = gen.gts(RW(gamma=1), N = n)
  
  # Store process
  out[i,] = Xt
}

# Plot random walks
plot(NA, xlim = c(1,n), ylim = range(out), xlab = "Time", ylab = " ")
grid()
color = sample(topo.colors(B, alpha = 0.5))
for (i in seq_len(B)){
  lines(out[i,], col = color[i])
}

# Add 95% confidence region
lines(1:n, 1.96*sqrt(1:n), col = 2, lwd = 2, lty = 2)
lines(1:n, -1.96*sqrt(1:n), col = 2, lwd = 2, lty = 2)

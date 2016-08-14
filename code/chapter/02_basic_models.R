## @knitr generate_white_noise
set.seed(1336)          # Set seed to reproduce the results
n  = 200                # Number of observations to generate
wn = ts(rnorm(n,0,1))   # Generate Guassian white noise.

autoplot(wn) +
  ggtitle("White Noise Process") +
  ylab("Displacement") + xlab("Time (seconds)")

## @knitr generate_drift
n     = 200             # Number of observations to generate
drift = .3              # Drift Control
dr    = ts(drift*(1:n)) # Generate drift sequence (e.g. y = drift*x + 0)

autoplot(dr) +
  ggtitle("Drift Process") +
  ylab("Displacement") + xlab("Time (seconds)")

## @knitr generate_ma1
set.seed(1345)  # Set seed to reproduce the results
n      = 200    # Number of observations to generate
sigma2 = 2      # Controls variance of Guassian white noise.
theta  = 0.3    # Handles the theta component of MA(1)


# Generate a white noise
wn = rnorm(n+1, sd = sqrt(sigma2))

# Simulate the MA(1) process
ma = rep(0, n+1)
for(i in 2:(n+1)) {		
  ma[i] = theta*wn[i-1] + wn[i]
}

ma = ts(ma[2:(n+1)])     # Remove first item

autoplot(ma) +
  ggtitle("Moving Average Order 1 Process") +
  ylab("Displacement") + xlab("Time (seconds)")

## @knitr generate_rw
set.seed(1336)       # Set seed to reproduce the results
n  = 200             # Number of observations to generate
w  = rnorm(n,0,1)    # Generate Guassian white noise.
rw = ts(cumsum(w))   # Cumulative sum

# Create a data.frame to graph in ggplot2
autoplot(rw) +
  ggtitle("Random Walk") +
  ylab("Displacement") + xlab("Time (seconds)")

## @knitr generate_rwd
set.seed(1336)       # Set seed to reproduce the results
n     = 200          # Number of observations to generate
drift = .3           # Drift Control

w = rnorm(n,0,1)     # Generate Guassian white noise.
wd = w + drift       # Add a drift
rwd = ts(cumsum(wd)) # Cumulative sum

# Create a data.frame to graph in ggplot2
autoplot(rwd) + 
  ggtitle("Random Walk with Drift") +
  ylab("Displacement") + xlab("Time (seconds)")

## @knitr compare_rw_and_rwd

# Add identifiers
drift.df = data.frame(Index = 1:n, Data = drift*(1:n), Type = "Drift")

rw.df = data.frame(Index = 1:n, Data = rw, Type = "Random Walk")

rwd.df = data.frame(Index = 1:n, Data = rwd, Type = "Random Walk with Drift")

combined.df = rbind(drift.df, rw.df, rwd.df)

ggplot(data = combined.df, aes(x = Index, y = Data, colour = Type)) +
  geom_line() +
  ggtitle("Comparisons of Random Walk") +
  ylab("Displacement") + xlab("Time (seconds)")

## @knitr generate_ar1
set.seed(1345)  # Set seed to reproduce the results
n      = 200    # Number of observations to generate
sigma2 = 2      # Controls variance of Guassian white noise.
phi    = 0.3    # Handles the phi component of AR(1)

wn = rnorm(n+1, sd = sqrt(sigma2))

# Simulate the MA(1) process
ar = rep(0,n+1)
for(i in 2:n) {		
  ar[i] = phi*ar[i-1] + wn[i]
}

ar = ts(ar[2:(n+1)])

autoplot(ar) +
  ggtitle("Autoregressive Order 1 Process") +
  ylab("Displacement") + xlab("Time (seconds)")
## @knitr example_JJ
# Load data
data(jj, package = "astsa")

# Construct gts object
jj = gts(jj, start = 1960, freq = 4, name = 'Johnson and Johnson Quarterly Earnings', 
         unit = "year")

# Plot time series
autoplot(jj) + ylab("Quarterly Earnings per Share ($)")


## @knitr example_EQ
# Load data
data(EQ5, package="astsa")
data(EXP6, package="astsa")

EQ5.df = fortify(EQ5)
EQ5.df$type = "earthquake"
EXP6.df = fortify(EXP6)
EXP6.df$type = "explosion"
eq.df = rbind(EQ5.df, EXP6.df)

# Plot time series
ggplot(data = eq.df, aes(Index, Data)) + geom_line() + facet_grid( type ~ .) +
  ylab("Ground Displacement (mm)") + xlab("Time (seconds)") + theme_bw()


## @knitr example_hydro
# Load data
hydro = read.csv("data/precipitation.csv", header=T, sep=";")

# Construct gts object
hydro = gts(hydro[,2], start = 1907, freq = 12, name = 'Precipitation Data', 
            unit = "month")

# Plot data
autoplot(hydro)  +  ylab("Mean Monthly Precipitation (mm)")


## @knitr example_Starbucks
library(timeDate)

# Load "high-frequency" Starbucks returns for Jul 01 2011
data(sbux.xts, package = "highfrequency")

# Plot returns
par(mfrow = c(1,2))
plot(sbux.xts[1:89], main = " ", ylab = "Returns")
plot(sbux.xts, main = " ", ylab = "Returns")


## @knitr example_IMU
# Load packages
library(gmwm)
library(imudata)

# Load IMU data
data(imu6, package = "imudata")

# Construct gst object
Xt = gts(imu6[,1], name = "Gyroscope data", unit = "hour", freq = 100*60*60)

# Plot time series
autoplot(Xt) + ylab(expression(paste("Error ", (rad/s^2))))

## @knitr example_WN
# This code simulates a gaussian white noise process
n = 1000                               # process length
sigma2 = 1                             # process variance
Xt = gen.gts(WN(sigma2 = sigma2), N = n)
plot(Xt)


## @knitr RW2d
library("gridExtra")

# Function computes direction random walk moves
RW2dimension = function(steps = 100){
  # Initial matrix
  step_direction = matrix(0, steps+1, 2)
  
  # Start random walk
  for (i in seq(2, steps+1)){
    # Draw a random number from U(0,1)
    rn = runif(1)
    
    # Go right if rn \in [0,0.25)
    if (rn < 0.25) {step_direction[i,1] = 1}
    
    # Go left if rn \in [0.25,0.5)
    if (rn >= 0.25 && rn < 0.5) {step_direction[i,1] = -1}
    
    # Go forward if rn \in [0.5,0.75)
    if (rn >= 0.5 && rn < 0.75) {step_direction[i,2] = 1}
    
    # Go backward if rn \in [0.75,1]
    if (rn >= 0.75) {step_direction[i,2] = -1}
  }
  
  # Cumulative steps
  position = data.frame(x = cumsum(step_direction[, 1]), y = cumsum(step_direction[, 2]))
  
  # Mark start and stop locations
  start_stop = data.frame(x = c(0, position[steps+1, 1]), y = c(0, position[steps+1, 2]),
    type = factor(c("Start","End"), levels = c("Start","End")))
  
  # Plot results
  ggplot(mapping = aes(x = x, y = y)) + geom_path(data = position) + 
    geom_point(data = start_stop, aes(color = type), size = 4) +
    theme_bw() + labs(x = "X-position", y = "Y-position",
    title = paste("2D random walk with", steps, "steps"),
    color = "") + theme(legend.position = c(0.15, 0.84))
}


# Plot 2D random walk with 10^2 and 10^5 steps
set.seed(5)
a = RW2dimension(steps = 10^2)
b = RW2dimension(steps = 10^4) 
grid.arrange(a, b, nrow = 1)


## @knitr example_RW
# This code simulates a gaussian random walk process
n = 1000                               # process length
gamma2 = 1                             # innovation variance
Xt = gen.gts(RW(gamma2 = gamma2), N = n)
plot(Xt)

## @knitr example_AR1
# This code simulate a gaussian random walk process
n = 1000                              # process length
phi = 0.5                             # phi parameter
sigma2 = 1                            # innovation variance
Xt = gen.gts(AR1(phi = phi, sigma2 = sigma2), N = n)
plot(Xt)

## @knitr example_MA1
# This code simulates a gaussian white noise process
n = 1000                              # process length
sigma2 = 1                            # innovation variance
theta = 0.5                           # theta parameter
Xt = gen.gts(MA1(theta = theta, sigma2 = sigma2), N = n)
plot(Xt)

## @knitr example_Drift
# This code simulate a linear drift with 0 intercept
n = 100                               # process length
omega = 0.5                           # slope parameter
Xt = gen.gts(DR(omega = omega), N = n)
plot(Xt)

## @knitr example_composite
n = 1000                                # process length
delta = 0.005                           # delta parameter (drift)
sigma2 = 10                             # variance parameter (white noise)
gamma2 = 0.1                            # innovation variance (random walk)
model = WN(sigma2 = sigma2) + RW(gamma2 = gamma2) + DR(omega = delta)
Xt = gen.lts(model, N = n)
plot(Xt)

## @knitr example_PSR
# Saving Rates
data("savingrt", package="smacdata")

# Plot time series
autoplot(savingrt) + ylab("US Personal Saving Rates (%)")

## @knitr numberline
number_line = function(named){
  d = data.frame(v = seq_along(named), o = rep(0, length(named)), named = named)
  
  ggplot(d, aes(x = v, y = o)) + geom_line() + 
  geom_segment(mapping = aes(xend = v, yend = 0, y = -.05)) +  # Replicate Tick marks
  geom_text(vjust = 2, aes(label = named)) +                   # Label axis marks
  coord_fixed(ylim = c(-0.5,0.5)) +                            # Suppress graph window changes
  theme(axis.line=element_blank(),                             # Disable everything...
  axis.text.x=element_blank(),
  axis.text.y=element_blank(),
  axis.ticks=element_blank(),
  axis.title.x=element_blank(),
  axis.title.y=element_blank(),
  legend.position="none",
  panel.background=element_blank(),
  panel.border=element_blank(),
  panel.grid.major=element_blank(),
  panel.grid.minor=element_blank(),
  plot.background=element_blank())
}

named = c(paste0("t = ", 1:2), "...", paste0("t = n - ", 2:1), "t = n")
number_line(named) 
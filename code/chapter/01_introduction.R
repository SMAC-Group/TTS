## @knitr example_AR1
# This code simulate a gaussian random walk process
n = 100                               # process length
phi = 0.5                             # phi parameter
sigma2 = 1                            # innovation variance
Xt = gen.gts(AR1(phi = phi, sigma2 = sigma2), n = n)
plot(Xt)

## @knitr RW2d

RW2dimension = function(steps = 100){
  # Initial matrix
  step_direction = matrix(0,steps+1,2)
  
  # Start random walk
  for (i in 2:steps+1){
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
  
  # Cumulate steps
  position = cbind(cumsum(step_direction[,1]),cumsum(step_direction[,2]))
  
  # Plot results
  plot(NA, xlim = range(position[,1]), ylim = range(position[,2]),
       xlab = "X-position", ylab = "Y-position",
       main = paste("2D random walk with", steps, "steps"))
  grid()
  lines(position, type = "l")
  points(c(0,position[steps,1]),c(0,position[steps,2]), 
         pch = 16, cex= 3, col = c("red","blue"))
  legend("topleft", c("Start point","End point"),
  pch = 16, pt.cex = 3, col = c("red","blue"), bty = "n",
  bg = "white", box.col = "white", cex = 1.2)
}

# Plot 2D random walk with 10^2 and 10^4 steps
set.seed(2)
par(mfrow = c(1,2))
RW2dimension(steps = 10^2)
RW2dimension(steps = 10^4) 

## @knitr example_WN
# This code simulate a gaussian white noise process
n = 100                               # process length
sigma2 = 1                            # process variance
Xt = gen.gts(WN(sigma2 = 1), n = n)
plot(Xt)

## @knitr example_RW
# This code simulate a gaussian random walk process
n = 100                               # process length
sigma2 = 1                            # innovation variance
Xt = gen.gts(RW(sigma2 = sigma2), n = n)
plot(Xt)

## @knitr example_highfreq
# Load packages
library(highfrequency)
library(timeDate)

# Load "high-frequency" Starbucks returns for Jul 01 2011
data(sbux.xts)

# Plot returns
par(mfrow = c(1,2))
plot(sbux.xts[1:89], main = " ", ylab = "Returns")
plot(sbux.xts, main = " ", ylab = "Returns")

## @knitr example_IMU
# Load packages
library(imudata)
library(gmwm)

# Load IMU data
load(imu6)

# Plot gryoscope on axis X
plot(imu6[1:100,1], type = "l", ylab = "Measurement error")

## @knitr example_jj
# Stock Data
data(jj)

autoplot(jj) +
  ggtitle("Johnson and Johnson Quarterly Earnings") +
  xlab("Year")  + ylab("Quarterly Earnings per Share")
 
## @knitr example_speech
# Speech information
data(speech)

autoplot(speech) +
  ggtitle("Speech Data") +
  xlab("Time")  + ylab("Speech")

## @knitr example_eq
# Earthquake
data(EQ5)
data(EXP6)

EQ5.df = fortify(EQ5)
EQ5.df$type = "earthquake"
EXP6.df = fortify(EXP6)
EXP6.df$type = "explosion"

eq.df = rbind(EQ5.df, EXP6.df)

ggplot(data = eq.df, aes(Index, Data)) +
  geom_line() +
  facet_grid( type ~ .) +
  ylab("Displacement") +
  xlab("Time (seconds)")
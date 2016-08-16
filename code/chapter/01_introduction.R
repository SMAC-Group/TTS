## @knitr example_WN
# This code simulate a gaussian white noise process
n = 100                               # process length
sigma2 = 1                            # process variance
Xt = gen.gts(WN(sigma2 = 1), n = n)
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
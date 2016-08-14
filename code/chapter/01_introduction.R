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
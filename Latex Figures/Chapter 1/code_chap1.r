## @knitr example_JJ
# Load data
data(jj, package = "astsa")

# Construct gts object
jj = gts(jj, start = 1960, freq = 4, name = 'Johnson and Johnson Quarterly Earnings', 
         unit = "year")

# Plot time series
autoplot(jj) + ylab("Quarterly Earnings per Share ($)")

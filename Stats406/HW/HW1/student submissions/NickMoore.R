##### Nick Moore #####
#### STATS 406 ####
### Homework 1 ###

## Problem 1.
# a.

# Z = sqrt(-2log(U1)) * cos(2*pi*U2)

# Generate the independent, uniform random variables; u1 and u2
U1 <- runif(n = 1000, min = 0, max = 1)
U2 <- runif(n = 1000, min = 0, max = 1)

# Create the vector Z of length n = 1000
vecZ <- sqrt(-2*log(U1)) * cos(2*pi*U2)

# b.

  # (a) Proportion of elements of vecZ that fall below 0
prop.below.zero <- sum(vecZ < 0) / length(vecZ)
prop.below.zero

  # (b) Proportion of elements of vecZ between 0 and 1
prop.between <- sum(vecZ >= 0 & vecZ <= 1) / length(vecZ)
prop.between

  # (c) Proportion of elements of vecZ above 1
prop.above.one <- sum(vecZ > 1) / length(vecZ)
prop.above.one

## Problem 2.
# Find Bn(x) where x = 0.2, n = 20
n <- 20;
x <- 0.2;
K <- c(0:n);
problem2.solution <- sum(abs(sin((2*pi*K)/n)) * choose(n, K) * x^K * (1 - x)^(n - K))
problem2.solution

## Problem 3.
load("FrontRange.RData")

# a. Find High/Low Elevation and plot daily rainfall for only those two
FR.maximum <- max(FR$info$elev)
FR.maximum
FR.maximum.index <- which(FR$info$elev == FR.maximum)

FR.minimum <- min(FR$info$elev)
FR.minimum
FR.minimum.index <- which(FR$info$elev == FR.minimum)

  # Max is red, Min is green
plot(x = FR$time[[FR.maximum.index]], y = FR$precip[[FR.maximum.index]], col = "red", main = 
  "Rainfall For Highest/Lowest Elevation Stations", xlab = "Date", ylab = "Daily Rainfall")
points(x = FR$time[[FR.minimum.index]], y = FR$precip[[FR.minimum.index]], col = "green")

#b. Find proportion of days with 10mm rain or more
prop.maximum.station <- sum(FR$precip[[FR.maximum.index]] > 10) / length(FR$precip[[FR.maximum.index]])
prop.maximum.station

prop.minimum.station <- sum(FR$precip[[FR.minimum.index]] > 10) / length(FR$precip[[FR.minimum.index]])
prop.minimum.station

#c. Find highest precipitation for any station (Ex: 10)
  # Takes station number as input, returns index 
  # that can be used to find date of highest precipitation
HighestPrecipitation <- function(station.number) {
  high.precip <- max(FR$precip[[station.number]])
  high.precip.index <- which(FR$precip[[station.number]] == high.precip)
  return(high.precip.index)
}

station.number <- 10
station.ten.index <- HighestPrecipitation(station.number)
station.ten.date <- FR$time[[station.number]][station.ten.index]
station.ten.date
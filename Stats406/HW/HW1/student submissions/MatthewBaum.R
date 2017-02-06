#Problem 1a
u1 = runif(n = 1000, min = 0, max = 1)
u2 = runif(n = 1000, min = 0, max = 1)
vecZ = sqrt(-2 * log(u1)) * cos(2 * pi * u2)
vecZ

#Problem 1b
below_zero = sum(vecZ < 0) / length(vecZ)
between = sum(vecZ >= 0 & vecZ <= 1) / length(vecZ)
above_one = sum(vecZ > 1) / length(vecZ)
below_zero; between; above_one;

#verify that the summation of the probabilities equals 1
sum(below_zero,between,above_one)

#Problem 2
B = function(n,x){
  k = c(0:n)
  x1 = abs(sin((2 *pi*k)/n))
  x2 = choose(n,k) * (x^k) * ((1-x)^(n-k))
  final = sum(x1 * x2)
  return(final)
}
result = B(n = 20, x = 0.2)
result

#Problem 3
setwd("/Users/mbaum727/Desktop")
load("FrontRange.RData")

#Problem 3a

#finds station at highest elevation
max_elevation = which(FR$info$elev == max(FR$info$elev))
max_elevation
plot(y = FR$precip[[max_elevation]], x = FR$time[[max_elevation]])

#finds station at lowest elevation
min_elevation = which(FR$info$elev == min(FR$info$elev))
min_elevation
plot(y = FR$precip[[min_elevation]], x = FR$time[[min_elevation]])

#Problem 3b

#Proportion of days with 10mm of rain or more at highest elevation
sum(FR$precip[[max_elevation]] >= 10) / length(FR$precip[[max_elevation]])

#Proportion of days with 10mm of rain or more at lowest elevation
sum(FR$precip[[min_elevation]] >= 10) / length(FR$precip[[min_elevation]])

#Problem 3c
#use which to find which date has the highest max
max_precip = function(station){
  index = which.max(FR$precip[[station]])
  max_date = FR$time[[station]][[index]]
  return(max_date)
}

max_precip(station = 10)

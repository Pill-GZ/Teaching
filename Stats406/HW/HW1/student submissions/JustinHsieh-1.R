#HW1 Justin Hsieh 15906450

#1a
U1 <- runif(n=1000, min=0, max=1)
U2 <- runif(n=1000, min=0, max=1)
vecZ <- sqrt(-2*log(U1))*cos(2*pi*U2)

#1b
length(which(vecZ < 0)) / 1000 #prop below 0: 0.499
length(which(vecZ > 0 & vecZ < 1)) / 1000 #prop between 0 and 1: 0.349
length(which(vecZ > 1)) / 1000 #prop above 1: 0.152

#2
x <- 0.2
n <- 20
sum(abs(sin(2*pi*(0:n) / 20)) * choose(n,(0:n)) * x^(0:n) * (1-x)^(n-(0:n))) # 0.809915

#3a
load("FrontRange.RData")
max_station = which.max(FR$info[['elev']])
min_station = which.min(FR$info[['elev']])
plot(FR$time[[max_station]], FR$precip[[max_station]], main="Highest Elevation Station Precipitation", xlab='Year', ylab='Precipitation(mm)')
plot(FR$time[[min_station]], FR$precip[[min_station]], main="Lowest Elevation Station Precipitation", xlab='Year', ylab='Precipitation(mm)')

#3b
length(which(FR$precip[[max_station]] > 10)) / length(FR$precip[[max_station]]) #prop with 10mm or more for max station: 0.1883302
length(which(FR$precip[[min_station]] > 10)) / length(FR$precip[[min_station]]) #prop with 10mm or more for min station: 0.09541528

#3c
station_num = 10
index = which.max(FR$precip[[station_num]])
FR$time[[station_num]][[index]] # 1999.592

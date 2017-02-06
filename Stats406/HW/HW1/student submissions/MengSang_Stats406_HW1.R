#Meng Sang
#STATS 406
#HW1

#Problem1
#1(a).
U1 <- runif(1000,0,1)
U2 <- runif(1000,0,1)
vecZ <- sqrt(-2 * log(U1)) * cos(2 * pi * U2)

#1(b).
#a)proportion of the elements of vecZ below 0:
count1 <- mean(vecZ < 0) 
print(count1) #0.495 of the elements of vecZ fall below 0.

#b)proportion of the elements of vecZ between 0 and 1:
count2 <- mean((vecZ > 0) & (vecZ < 1)) 
print(count2) #0.334 of the elements of vecZ fall between 0 and 1.

#c)proportion of the elements of vecZ above 1:
count3 <- mean(vecZ > 1) 
print(count3) #0.174 of the elements of vecZ fall above 1.

#Problem2
n=20
k <- 0:n
B <- function(x){
  sum(abs(sin(2*pi*k/n))*choose(n,k)*(x^k)*(1-x)^(n-k))
}
B(0.2) #B(0.2) = 0.8099159

#Problem3
#3(a).Find the station at the highest elevation and the station at the lowest elevation:
load("FrontRange.RData")
elv = FR$info[3]
highest_station = which(elv == max(elv))
cat("station", highest_station, "is at the highest elevation. \n" )
#station 36 is at the highest elevation.

lowest_station = which(elv == min(elv))
cat("station", lowest_station, "is at the lowest elevation. \n" )
#station 27 is at the lowest elevation.

#plot daily rainfalls over time of these two stations
plot(FR$time[[highest_station]],FR$precip[[highest_station]])
plot(FR$time[[lowest_station]],FR$precip[[lowest_station]])

#3(b).
#The proportion of days with 10mm of rain for station at the lowest elevation:
count_lowest <- mean(FR$precip[[lowest_station]] >= 10) 
print(count_lowest) #returns 0.1132226

#The proportion of days with 10mm of rain for station at the highest elevation:
count_highest <- mean(FR$precip[[highest_station]] >= 10)
print(count_highest) #returns 0.262334

#3(c).For a given station, the date of the record highest amount of precipitation:
station_index = 10
max_precip = which(FR$precip[[station_index]] == max(FR$precip[[station_index]]))
FR$time[[station_index]][max_precip]  #returns 1999.592   
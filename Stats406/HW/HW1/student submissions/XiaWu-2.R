###########################
#1.
#(a)
U1 <- runif(1000,0,1)
U2 <- runif(1000,0,1)
vecZ <- sqrt(-2 * log(U1)) * cos(2 * pi * U2)
vecZ

#(b)(a)
#The proportion below 0
index_sel <- which(vecZ < 0)
length(index_sel) / length(vecZ)


#(b)
#The proportion between 0 and 1
index_sel <- which(vecZ > 0 & vecZ < 1)
length(index_sel)/length(vecZ)

#(c)
#The proportion above 1
index_sel <- which(vecZ > 1)
length(index_sel)/length(vecZ)

###########################
#2.
k = c(0:20)
sum( abs(sin((2*pi*k)/20)) * choose(20,k) * (0.2^k) * (0.8)^(20-k) )

###########################
#3.
#(a)
load("/Users/wuxia/Desktop/FrontRange.RData")
##Find the lowest elevation
lowest_elevation <-which.min(FR$info[[3]])
lowest_elevation
station_name <- names(FR$time)[lowest_elevation]
station_name

##Find the highest elevation
highest_elevation <- which.max(FR$info[[3]])
highest_elevation
station_name <- names(FR$time)[highest_elevation]
station_name

##Plot the daily rainfalls over timme
#Plot the graph of station with the lowest elevation
plot(FR$time[[lowest_elevation]],FR$precip[[lowest_elevation]],xlab ='date', ylab ='daily rainfalls')

#Plot the graph of station with the highest elevation
plot(FR$time[[highest_elevation]],FR$precip[[highest_elevation]],xlab ='date', ylab ='daily rainfalls')

#(b)
##Calculate the proportion for the station with lowest elevatoin
sta_lowest <- which(FR$precip[[lowest_elevation]] >= 10)
prop_lowest <- length(sta_lowest) / length(FR$precip[[lowest_elevation]])
prop_lowest

##Calculate the proportion for the station with highest elevatoin
sta_highest <- which(FR$precip[[highest_elevation]] >= 10)
prop_highest <- length(sta_highest) / length(FR$precip[[highest_elevation]])
prop_highest

#(c)
highest_precip <- which.max(FR$precip[[10]])
highest_precip
data <- FR$time[[10]][highest_precip]
data


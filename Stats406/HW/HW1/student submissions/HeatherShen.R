# 1
#1a
u_1 = runif(n = 1000, min = 0, max = 1) #creates random uniform variable
u_2 = runif(n = 1000, min = 0, max = 1) #creates second random uniform variable
vecZ = sqrt(-2*log(u_1))*cos(2*pi*u_2) #creates vector vecZ using the 2 uniform variables
#1b
below = (vecZ < 0) #assigns TRUE to all elements less than 0 in vecZ
between = (0 < vecZ) & (vecZ < 1) #assigns TRUE to all elements between 0 and 1 in vecZ
above = (vecZ > 1) #assigns TRUE to all elements greater than 1 in vecZ
mean(below) #returns proportion of elements less than 0 in vecZ
mean(between) #returns proportion of elements between 0 and 1 in vecZ
mean(above) #returns proportion of elements greater than 1 in vecZ

# 2
n = 20
x = 0.2
k = 1:n #creates vector 1:20
a = abs(sin((2*pi*k)/n)) #creates vector with 20 elements
b = choose(n,k) #creates vector with 20 elements
c = x^k #creates vector with 20 elements
d = (1-x)^(n-k) #creates vector with 20 elements
B = sum(a*b*c*d) #multiplies a*b*c*d for every element and then sums up all elements

# 3
#3a
load("FrontRange.RData")
max_elev = max(FR$info[["elev"]]) #returns max elevation level
max_elev_i = which(FR$info[["elev"]] == max_elev) #returns which station has max elevation level
min_elev = min(FR$info[["elev"]]) #returns min elevation level
min_elev_i = which(FR$info[["elev"]] == min_elev) #returns which station has min elevation levels
#plot for max elevation station, time on x-axis, precipitation level on y-axis
plot(FR$time[[max_elev_i]], FR$precip[[max_elev_i]], 
     main = "Daily Rainfalls over Time", xlab = "Time", ylab = "Daily Rainfall")
#plot for min elevation station, time on x-axis, precipitation level on y-axis
plot(FR$time[[min_elev_i]], FR$precip[[min_elev_i]], 
     main = "Daily Rainfalls over Time", xlab = "Time", ylab = "Daily Rainfall")

#3b
max_prop = mean(FR$precip[[max_elev_i]] > 10) 
#returns proportion of days where precip level at max elevation station was greater than 10
min_prop = mean(FR$precip[[min_elev_i]] > 10)
#returns proportion of days where precip level at min elevation station was greater than 10

#3c
i = 10 #example of station 10
max_precip = max(FR$precip[[i]]) # i = any given station number, returns max precip level for station
max_precip_i = which(FR$precip[[i]] == max_precip) # returns which index number of max precip level
max_precip_date = FR$time[[i]][max_precip_i] # returns date for max precip level using index number

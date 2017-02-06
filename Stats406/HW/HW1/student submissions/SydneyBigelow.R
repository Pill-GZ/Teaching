#Sydney Bigelow
#Homework 1
#09/22/2016

#1.a
U1 = runif(1000, min=0, max=1) 
U2 = runif(1000, min=0, max=1)
vecZ = c(sqrt((-2*log(U1)))*cos(2*pi*U2))

#1.b
#below 0
z_below_0 = mean(vecZ<0)
z_below_0
#between 0 and 1
z_between_0_and_1 = mean(vecZ>0 & vecZ<1)
z_between_0_and_1
#above 1
z_above_1 = mean(vecZ>1)
z_above_1

#2
x = 0.2
n = 20
vec_k = c(0:20)
part_1 = abs(sin(((2*pi*vec_k)/n)))
n_choose_k = choose(20,vec_k)
vec_x = c(x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x)
x_power_of_k = vec_x^(vec_k)
n_minus_k = n - vec_k
subtract_x = (1 - vec_x)
to_the_power = subtract_x^(n_minus_k)
B = part_1 * n_choose_k * x_power_of_k *to_the_power
sum(B)

#3.a
load("/Users/sydneybigelow/Documents/STATS 406/FrontRange.RData")
elevation = FR$info[3] #elevation

#Station at MAX elevation:
max(elevation)
which(elevation == 10260)
y_max = FR$precip[[36]] #daily rainfall for 36th station
x_max = FR$time[[36]] #time for the 36th station
max_name = names(FR$precip[36]) #station name
length(y_max);length(x_max) #make sure that both are the same length
plot(x_max, y_max, type = 'l', col="hotpink",xlab = "Maximum Elev Station Time", ylab = "Maximum Elev Station Precipitation")

#Station at MIN elevation:
min(elevation)
which(elevation == 4650) #true at 27
min_name = names(FR$precip[27]) #find the name of the station 
y_min = FR$precip[[27]]
x_min = FR$time[[27]]
length(y_min);length(x_min) #make sure they are the same length
plot(x_min, y_min, type = 'l', col="blue", xlab = "Minimum Elev Station Time", ylab = "Minimum Elev Station Precip")

#3.b
#Proportion of days with 10mm of rain or more for MAX elevation station
above_ten_max = FR$precip[[36]] >=10
mean(above_ten_max)
#Proportion of days with 10mm of rain or more for MIN elevation station
above_ten_min = FR$precip[[27]] >=10
mean(above_ten_min)

#3.c
#write code to find for a given station (i) the date of the record highest amount of precipitation

#Change i, where i is the station that you want
#here, I wrote code for station 10 (but i can be changed)
i = 10 
station_i = FR$precip[[i]]
max_station_i = max(FR$precip[[i]])
max(station_i)
station_we_want = which(station_i == max_station_i)
FR$time[[i]][station_we_want] #This outputs date


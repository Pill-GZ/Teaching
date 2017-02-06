Moise Mouyebe
Stats 406
Homework#1





#**********
#Problem 1
#**********


#######(a)

U1 = runif(1000,0,1);
U2 = runif(1000,0,1);
vecZ = sqrt(-2*log(U1))*cos(2*pi*U2)


#######(b)

##The proportion of elements of vecZ that fall

##(a) Below 0:

sum(vecZ < 0)/1000

##(b) Between 0 and 1:

sum((vecZ > 0) & (vecZ < 1)) / 1000

##(c) Above 1:

sum(vecZ > 1)/1000








#**********
#Problem 2
#**********

n = 20;
x = 0.2;
v = 0:n;

first = abs(sin((2*pi*v)/n));
second = choose(n,v);
third = (x^v)*(1-x)^(n-v);
B_n_x = sum(first*second*third);
B_n_x = 0.8099159






#**********
#Problem 3
#**********
  #######(a)
  
  The station with the lowest elevation is:
  which(FR$info$elev==min(FR$info$elev)) = 27
  
   The station with the highest elevation is:
   which(FR$info$elev==max(FR$info$elev)) = 36
   
   Plot of the daily rainfalls at station #27
   plot(lowest_elev_station_time,lowest_elev_station_precipitation,col = "red", ylim = c(0,400))
   
    Plot of the daily rainfalls at station #36
    plot(highest_elev_station_time,highest_elev_station_precipitation,col = "red")
    
  #######(b)
  
  Proportion of days with 10mm of rain at station #27
  sum(FR$precip[[27]] >= 10)/length(FR$precip[[27]]) = 0.1132226
  
  Proportion of days with 10mm of rain at station #36
  sum(FR$precip[[36]] >= 10)/length(FR$precip[[36]]) = 0.262334
  
  
  #########(c)
  
#This script returns the date of the record highest amount of precipitation in 
#a given station
##---------------------------------------------##
#This script assumes that the data set FR is already loaded in the workspace

n = 10  ### The station number
index = which(FR$precip[[n]] == max(FR$precip[[n]]));   ###This returns the index at which the highest amount of precipitation was recorded.
record_date = FR$time[[n]][index];  ### This is the date at which it occured
record_date
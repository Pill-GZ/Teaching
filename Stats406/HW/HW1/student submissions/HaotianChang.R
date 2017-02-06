# Haotian Chang 
# STATS 406
# HW 1

#----------------------------------------------------------------------------------------------------------#
#1.(a)
U1 = runif(1000, min = 0, max = 1); # Creat U1 who is uniform distributed on (0,1)
U2 = runif(1000, min = 0, max = 1); # Creat U2 who is uniform distributed on (0,1)
vecZ = numeric(1000); # Creat vector Z
vecZ = sqrt(-2*log(U1))*cos(2*pi*U2); # Assign values to Z and Z is N(0,1), Check mean(vecZ) & sd(vecZ).

#1.(b)
#(a)
mean(vecZ < 0); #the proportion of the elements of vecZ that fall below 0
#(b)
mean(vecZ < 1 & vecZ > 0); #the proportion of the elements of vecZ that fall between 0 and 1
#(c)
mean(vecZ > 1); #the proportion of the elements of vecZ that fall above 1

#----------------------------------------------------------------------------------------------------------#
#2
x = 0.2; #initialize x = 0.2
n = 20;  #initialize n = 20
k = seq(0,n,1); #Creat a vector k whose elements are from 0 to 20
B = numeric(21); #Creat vector B
B = abs(sin(2*pi*k/n))*choose(n,k)*x^(k)*(1-x)^(n-k); #expression
Bn = sum(B); #calculate the Bn(0.2)

#----------------------------------------------------------------------------------------------------------#
#3
load("C:/Users/mypcname/Desktop/FrontRange.RData");
#a.
which.max(FR$info[[3]]); #Find the station at the highest elevation
which.min(FR$info[[3]]); #Find the station at the lowest elevation
plot(FR$time[[which.max(FR$info[[3]])]], FR$precip[[which.max(FR$info[[3]])]],
     main='Daily rainfalls over time recorded by the 36th station', xlab = 'Time', ylab = 'Daily Rainfall'); #plot as title described
plot(FR$time[[which.min(FR$info[[3]])]], FR$precip[[which.min(FR$info[[3]])]],
     main='Daily rainfalls over time recorded by the 27th station', xlab = 'Time', ylab = 'Daily Rainfall'); #plot as title described

#b.
mean(FR$precip[[which.max(FR$info[[3]])]] >= 10); #the Proportion that days with 10mm of rain or more for station 36
mean(FR$precip[[which.min(FR$info[[3]])]] >= 10); #the Proportion that days with 10mm of rain or more for station 27

#c.
which.max(FR$precip[[10]]); #Find the position of the highest amount of precipitation of station 10
FR$time[[10]][which.max(FR$precip[[10]])]; #locate the actual date of the record highest amount of precipitation.
#----------------------------------------------------------------------------------------------------------------#








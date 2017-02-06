##STAT 406: HW1
##Weronika Filarska

##Problem 1:
U1 = runif(1000);
U2 = runif(1000); 
vecZ = sqrt(-2*log(U1))*cos(2*pi*U2);
vecZ;
mean( vecZ < 0);
mean( (vecZ>0) & (vecZ<1)); 
mean( vecZ>1);

##Problem 2:
x = 0.2; 
n = 20;
k = c(0:20) 
k
bin_a = abs(sin(2*pi*k/n))
bin_b = choose(n,k)
bin_c = x^k
bin_d = (1-x)^(n-k)
sum(abs(bin_a)*bin_b*bin_c*bin_d) 

##Problem 3 (a):
load(FR.RData)
load(FR)
(FR$precip)
(FR$info)
(FR$info$elev)
max(FR$info$elev)
min(FR$info$elev)

##find the highest elevation & lowest elevation
high_elevstation = max(FR$info$elev);
low_elevstation = min(FR$info$elev);

##find the station at the highest elevation & lowest elevation
h_index = which(FR$info[3] == high_elevstation)
l_index = which(FR$info[3] == low_elevstation)

##times corresponding to highest & lowest elevation stations
highstation_time = (FR$time[[h_index]])
highstation_time
lowstation_time = (FR$time[[l_index]])

##finding precipitation data for those specific stations
precip_high = (FR$precip[[h_index]])
precip_low = (FR$precip[[l_index]])

##Plot 
plot(highstation_time,precip_high,xlab = "Max Station Time", ylab = "Rainfall")
plot(lowstation_time,precip_low,xlab = "Min Station Time", ylab = "Rainfall")

##Problem 3 (b):
mean(precip_high>=10)
mean(precip_low>=10)

##Problem 3 (c):
num_station = 10; 

## all precipitation vector values
all_precip = (FR$precip[[num_station]])
max_precip = max(all_precip)
max_index = which(all_precip == max_precip)
max_index

##all times for station number 10
all_times = (FR$time[[num_station]])
all_times
all_precip

##sanity check to see the length of vectors is the same
length(all_times)
length(all_precip)
all_times[[max_index]]
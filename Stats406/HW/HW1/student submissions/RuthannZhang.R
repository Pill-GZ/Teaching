### 1a ###
n <- 1000;
u1 <- runif(n, min = 0, max = 1);
u2 <- runif(n, min = 0, max = 1);
vecZ <- (sqrt(-2*log(u1)))*cos(2*pi*u2);

### 1b ###
mean(vecZ < 0); #proportion below 0 = 0.493
mean(vecZ >= 0 & vecZ <= 1); #proportion between 0 and 1 = 0.333
mean(vecZ > 1); #proportion above 1 = 0.174

### 2 ###
x <- 0.2;
n <- 20;
k <- 0:20;
Bnx <- abs(sin((2*pi*k)/n))*choose(n,k)*(x^k)*((1-x)^(n-k));
sum(Bnx); #0.8099159

### 3a ###
load("/Users/mac/Downloads/FrontRange.RData");
which.max(FR$info$elev); #row 36 contains the max elevation
which.min(FR$info$elev); #row 27 contains the min elevation
plot(FR$time[[36]], FR$precip[[36]]); #plot for max elevation station
plot(FR$time[[27]], FR$precip[[27]]); #plot for min elevation station

### 3b ###
mean(FR$precip[[36]] >= 10); #proportion for max elevation station = 0.262334
mean(FR$precip[[27]] >= 10); #proportion for min elevation station = 0.1132226

### 3c ###
station_x <- 10; #station number, can be changed
max_precip <- which.max(FR$precip[[station_x]]); #gets the index number for the max precip value
max_precip_date <- FR$time[[station_x]][max_precip]; #uses the index number obtained above to get the date when that rainfall occurred

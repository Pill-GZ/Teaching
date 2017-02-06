#problem 1a
U1 = runif(n = 1000, min = 0, max = 1)
U2 = runif(n = 1000, min = 0, max = 1)
vecZ = sqrt(-2*log(U1))*cos(2*pi*U2)


#problem 1b
mean(vecZ < 0) #0.526
mean(vecZ > 0 & vecZ < 1) #0.311
mean(vecZ > 1) #0.163

#problem 2
k = seq(from = 0, to = 20, by = 1)
sum(abs(sin((2*pi*k)/20))*choose(20,k)*(0.2^k)*((1-0.2)^(20-k))) #0.8099159

#problem 3a
elev = FR$info[3]
max(elev) #10260
which(elev == 10260) #36
min(elev) #4650
which(elev == 4650)#27
plot(FR$time[[36]], FR$precip[[36]])
plot(FR$time[[27]], FR$precip[[27]])

#problem 3b
station_36_precipitation = FR$precip[[36]]
mean(station_36_precipitation >= 10) #0.262334
station_27_precipitation = FR$precip[[27]]
mean(station_27_precipitation >= 10) #0.1132226

#problem 3c
max(FR$precip[[10]]) #399
which(FR$time[[10]]==399) #integer(0)
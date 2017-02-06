# Problem 1a
U1 = runif(n=1000, min=0, max=1)
U2 = runif(n=1000, min=0, max=1)
vecZ = sqrt(-2*log(U1))*cos(2*pi*U2)
# Problem 1b
mean(vecZ < 0) #0.474
mean(vecZ > 0 & vecZ < 1) #0.374
mean(vecZ > 1) #0.152

#Problem 2
k = seq(from = 0, to = 20, by = 1)
sum(abs(sin((2*pi*k)/20)*choose(20,k)*0.2^(k)*(1-0.2)^(20-k))) #0.8099159


#Problem 3a
elevation <- FR$info[3]
max(elevation) #10260
which(elevation==10260) #36
min(elevation) #4650
which(elevation==4650) #27
plot(FR$time[[36]], FR$precip[[36]])
plot(FR$time[[27]], FR$precip[[27]])

#3b
mean(FR$precip[[36]] >= 10) #0.262334
mean(FR$precip[[27]] >= 10) #0.1132226

#3c
max(FR$precip[[10]]) #399
which(FR$time[[10]] == 399) #integer(0)


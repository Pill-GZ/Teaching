# 1a
u1=runif(1000, min=0, max =1)
u1
u2=runif(1000, min=0, max =1)
u2
vecZ=sqrt(-2*log(u1))*cos(2*pi*u2)
vecZ

# 1ba

mean(vecZ<0)

#1bb
mean(0<vecZ & vecZ<1)

#1bc
mean(vecZ>1)


#2

x=.2
n=20
k=0:20
B=abs(sin((2*pi*k)/n))*choose(n,k)*(x^k)*(1-x)^(n-k)
B
sum(B)


#3a
load("/Users/mjrotter/Downloads/FrontRange.RData")
stationinfo=FR$info
which(stationinfo$elev==max(stationinfo$elev))
which(stationinfo$elev==min(stationinfo$elev))
plot(FR$time[[36]],FR$precip[[36]],main = "Highest Elevation Precipitation")
plot(FR$time[[27]],FR$precip[[27]],main = "Lowest Elevation Precipitation")

#3b
sum(FR$precip[[36]]>=10)/length(FR$precip[[36]])
sum(FR$precip[[27]]>=10)/length(FR$precip[[27]])

#3c
stationnumber=10
date=which(FR$precip[[stationnumber]]==max(FR$precip[[stationnumber]]))
FR$time[[stationnumber]][[date]]

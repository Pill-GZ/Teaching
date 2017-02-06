#Q1
#(a)
U1<-runif(n=1000,min=0,max=1)
U2<-runif(n=1000,min=0,max=1)
Z<-sqrt(-2*log(U1))*cos(2*pi*U2)
#(b)
#below zero
x1<-length(Z[Z<0])/1000
#between 0 and 1
x2<-length(Z[Z>0 & Z<1])/1000
#above 1
x3<-length(Z[Z>1])/1000

#Q2
k=c(1:20)
b<-abs(sin(2*pi*k/20))*choose(20,k)*(0.2^k)*((0.8)^(20-k))
sum_b <- sum(b)

#Q3
load('~/Desktop/stats 406/hw/FrontRange.RData')
#(a)
max_index<-which.max(FR$info[,3])
min_index<-which.min(FR$info[,3])
plot(FR$time[[max_index]],FR$precip[[max_index]],type='h',xlab='dates',ylab='rainfall')
plot(FR$time[[min_index]],FR$precip[[min_index]],type='h',xlab='dates',ylab='rainfall')
#(b)
precip_max <- FR$precip[[max_index]]
total_max <- length(precip_max)
p1 <- length(precip_max[precip_max>=10]) / total_max

precip_min <- FR$precip[[min_index]]
total_min <- length(precip_min)
p2 <- length(precip_max[precip_min>=10]) / total_min
#(c)
max_prep <- which.max(FR$precip[[10]])
date_max <- FR$time[[10]][max_prep]

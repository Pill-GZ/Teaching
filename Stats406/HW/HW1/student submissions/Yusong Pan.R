#problem1
#(a) 
U1 <- runif(1000,min=0,max=1)
U2 <- runif(1000,min=0,max=1)
vecZ <- sqrt(-2*log(U1))*cos(2*pi*U2)

#(b)
mean(vecZ<0)
mean((vecZ>=0)&(vecZ<=1))
mean(vecZ>1)

#problem2

B <- function(x){
n <- 20; k <- c(1:n); abs(sin((2*pi*k)/n))*choose(n,k)*(x^k)*((1-x)^(n-k))
}
sum(B(0.2))

#problem3
#(a)
load("/Users/yusongpan/Downloads/FrontRange.RData")
highest_elev <- which.max(FR$info[,3])
lowest_elev <- which.min(FR$info[,3])
plot(FR$precip[[lowest_elev]],FR$time[[lowest_elev]],type='l',xlab="time",ylab="precipitation",main="station 27")
plot(FR$precip[[highest_elev]]~FR$time[[highest_elev]],type='l',xlab="time",ylab="precipitation",main="station 36")

#(b)
mean(FR$precip[[lowest_elev]]>=10)
mean(FR$precip[[highest_elev]]>=10)

#(c)
find_highest_precip <- function(i){
FR$time[[i]][which.max(FR$precip[[i]])]
}
find_highest_precip(10)



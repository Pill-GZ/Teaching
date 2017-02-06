#Problem 1,(a)
U1 <- runif(n=1000,min=0,max=1)
U2 <- runif(n=1000,min=0,max=1)
vecZ <- sqrt((-2)*log(U1))*cos(2*pi*U2)

#Problem 1,(b)
mean(vecZ<0)
mean((vecZ>0)&(vecZ<1))
mean(vecZ>1)

#Problem 2
n<-20
k<-0:20
B <- function(x){
  sum(abs(sin(2*pi*k/n))*choose(n,k)*x^(k)*(1-x)^(n-k))
}
B(0.2)#returns 0.8099159

load('FrontRange.RData')
#Problem 3,(a)
FR$info
a <- which.max(FR$info[[3]])
print(a)#returns 36
b <- which.min(FR$info[[3]])
print(b)#returns 27

plot(FR$time[[a]],FR$precip[[a]])
plot(FR$time[[b]],FR$precip[[b]])

#Problem 3,(b)
mean(FR$precip[[a]]>=10)#returns 0.262334
mean(FR$precip[[b]]>=10)#returns 0.1132226

#Problem 3,(c)
c <- which.max(FR$precip[[10]])
FR$time[[10]][c]#returns 1999.592


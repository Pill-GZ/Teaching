#Q1
U1 = runif(1000,0,1)
U2 = runif(1000,0,1)
Z = sqrt((-2)*log(U1))*cos(2*pi*U2)

mean(Z < 0)
mean((Z >= 0) & (Z <= 1))
mean(Z > 1)

#Q2
k= 1:20
n = 20
x = 0.2
B = abs(sin((2*pi*k)/n))*choose(n,k)*(x^k)*(1-x)^(n-k)
B = sum(B)
print (B)


#Q3
load("/Users/Joyce/Downloads/FrontRange.RData")
which.max(FR$info$elev)
plot(FR$precip[[36]]~FR$time[[36]],type = 'l',col = 'red',xlab="time",ylab="precip")
which.min(FR$info$elev)
plot(FR$precip[[27]]~FR$time[[27]],type = 'l',col = 'red',xlab="time",ylab="precip")
rain36=FR$precip[[36]]>=10
mean(rain36)
rain27=FR$precip[[27]]>=10
mean(rain27)
highest_precip <- function(i){
  FR$time[[i]][which.max(FR$precip[[i]])]
}
i= 10
highest_precip(10)

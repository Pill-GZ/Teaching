# Problem 1, part (a):
U1 <- runif(n=1000, min=0, max=1)
U2 <- runif(n=1000, min=0, max=1)
Z <- (sqrt(-2*log(U1)))*cos(2*pi*U2) 
vecZ <- c(Z)
#
# Problem 1, part (b):
# (a) below 0
mean(vecZ<0)
# (b) between 0 and 1
mean((vecZ>0)&(vecZ<1))
# (c) above 1
mean(vecZ>1)
#
# Problem 2:
k <- 0:20
n <- 20
x <- .2
B <- abs(sin((2*pi*k)/n))*choose(n, k)*(x ^ k)*((1 - x)^(n - k))
vecB <- sum(B)
#
# Problem 3, part (a):
elev <- FR$info[[3]]
high_elev <- max(elev)
low_elev <- min(elev)
high_stat <- which(FR$info$elev == high_elev)
low_stat <- which(FR$info$elev == low_elev)
plot(x = FR$time[[high_stat]], y = FR$precip[[high_stat]], xlab = "Time", ylab = "Precipitation")
plot(x = FR$time[[low_stat]], y = FR$precip[[low_stat]], xlab = "Time", ylab = "Precipitation")
# 
# Problem 3, part (b):
mean(FR$precip[[high_stat]] >= 10)
mean(FR$precip[[low_stat]] >= 10)
#
# Problem 3, part (c):
stat_num = 10
max_precip <- max(FR$precip[[stat_num]])
max_precip_num <- which(FR$precip[[stat_num]] == max_precip)
date_max_precip <- FR$time[[stat_num]][[max_precip_num]]
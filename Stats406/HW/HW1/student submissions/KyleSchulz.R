#1A
vecZ <- (sqrt(-2 * log(runif(n = 1000))) * cos(2 * pi * runif(n = 1000)))
#1B part A
length(vecZ[vecZ < 0]) / length(vecZ)
#1B part B
length(vecZ[vecZ >= 0 & vecZ <= 1]) / length(vecZ)
#1B part C
length(vecZ[vecZ > 1]) / length(vecZ)
#2
n <- 20
k <- c(0:20)
x <- 0.2
sum(abs(sin((2 * pi * k) / n)) * choose(n, k) * (x^k) * ((1-x)^(n-k)))
#3a
load('FrontRange.RData')
FRinfo <- FR$info
maxElev <- max(FRinfo$elev)
which(FRinfo$elev == maxElev)
minElev <- min(FRinfo$elev)
which(FRinfo$elev == minElev)
plot(FR$precip[[which(FRinfo$elev == maxElev)]])
plot(FR$precip[[which(FRinfo$elev == minElev)]])
#3B
length(which(FR$precip[[which(FRinfo$elev == maxElev)]] >= 10)) / length(FR$precip[[which(FRinfo$elev == maxElev)]])
length(which(FR$precip[[which(FRinfo$elev == minElev)]] >= 10)) / length(FR$precip[[which(FRinfo$elev == minElev)]])
givenStation <- 10
FR$time[[givenStation]][[which(FR$precip[[givenStation]] == max(FR$precip[[givenStation]]))]]
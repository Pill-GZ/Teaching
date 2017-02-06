#####PROBLEM 1
##Part a
U1 = runif(1000, min = 0, max = 1)
U2 = runif(1000, min = 0, max = 1)
vecZ = (sqrt(-2*log(U1))) * (cos(2*pi*U2))
##Part b
#a - Portion below 0
sum(vecZ<0) / length(vecZ)
#b - Portion between 0 and 1
sum((vecZ>=0)&(vecZ<=1))/length(vecZ)
#c - Portion above 1
sum(vecZ>1) / length(vecZ)

#####PROBLEM 2
n = 20
x = 0.2
ks = 1:n
sum( abs(sin((2*pi*ks)/n)) * (choose(n, ks)) * (x^ks) * ((1-x)^(n-ks)) )

#####PROBLEM 3
load('FrontRange.RData')
##Part a
#Highest elevation
highest_idx = which.max((FR$info)$elev)
plot(FR$time[[highest_idx]], FR$precip[[highest_idx]])
#Lowest elevation
lowest_idx = which.min((FR$info)$elev)
plot(FR$time[[lowest_idx]], FR$precip[[lowest_idx]])
##Part b
#Highest elevation
sum(FR$precip[[highest_idx]] >= 10) / length(FR$precip[[highest_idx]])
#Lowest elevation
sum(FR$precip[[lowest_idx]] >= 10) / length(FR$precip[[lowest_idx]])
##Part c
most_percip_idx = which.max(FR$precip[[10]])
FR$time[[10]][[most_percip_idx]]
##### Question 1 #####
##A
U1 = runif(1000, min = 0, max = 1)
U2 = runif(1000, min = 0, max = 1)
vecZ = sqrt(-2*log(U1))*cos(2*(pi)*(U2))
##B
below0 = vecZ[vecZ < 0]
btwn0and1 = vecZ[vecZ > 0 & vecZ < 1]
above1 = vecZ[vecZ > 1]

##### Question 2 #####
B = function(x, n){
  vecB = seq(from = 0, to = n)
  sum(abs(sin(2*pi*vecB / n)) * choose(n, vecB) * x^vecB * (1-x)^(n-vecB))
}

B(.2, 20)

##### Question 3 #####
load("/Users/craigmarkman/Downloads/FrontRange.RData")

##(A)
elevmax = which.max(FR$info$elev)
elevmin = which.min(FR$info$elev)
plot(FR$precip[[which.max(FR$info$elev)]], FR$time[[which.max(FR$info$elev)]])
plot(FR$precip[[which.min(FR$info$elev)]], FR$time[[which.min(FR$info$elev)]])

##(B)
max_Greater_10 = sum(FR$precip[[elevmax]] >= 10) / length(FR$precip[[elevmax]])
min_Greater_10 = sum(FR$precip[[elevmin]] >= 10) / length(FR$precip[[elevmin]])

##(C)
recordHigh = function(s){
  highest = which.max(FR$precip[[s]])
  FR$time[[s]][[highest]]
}

recordHigh(10)

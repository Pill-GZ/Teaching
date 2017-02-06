####################
######Question 1
###(a)
U1 <- runif(1000, 0, 1)
U2 <- runif(1000, 0, 1)
vecZ <- sqrt((-2)*log(U1))*cos(2*pi*U2)

###(b)
num_vecZsmall <- vecZ[vecZ < 0]
num_vecZbetween <- vecZ[vecZ > 0 & vecZ < 1]
num_vecZlarge <- vecZ[vecZ > 1]
proportion_small <- length(num_vecZsmall) / length(vecZ)
proportion_between <- length(num_vecZbetween) / length(vecZ)
proportion_large <- length(num_vecZlarge) / length(vecZ)
proportion_small
proportion_between
proportion_large


####################
######Question 2
B=function(x,n){
  a <- abs(sin(2*pi*(0:n)/n))
  b <- choose(n,(0:n))
  c <- x^(0:n)
  d <- (1-x)^(n-(0:n))
  sum(a*b*c*d)
}
B(0.2,20)


####################
######Question 3
###(a)
load('FrontRange.Rdata')
e <- FR$info[[3]]
z <- which(e == max(e))
z
#z gives the station which have the max precipitation
z1 <- which(e == min(e))
z1
#z1 gives the station which have the min precipitation
plot(FR$time[[z1]], FR$precip[[z1]])
plot(FR$time[[z]], FR$precip[[z]])

###(b)
rainfall_z1 <- FR$precip[[z1]]
num_z1 <- rainfall_z1[rainfall_z1 >= 10]
proportion_z1 <- length(num_z1) / length(rainfall_z1)
proportion_z1

rainfall_z <- FR$precip[[z]]
num_z <- rainfall_z[rainfall_z >= 10]
proportion_z <- length(num_z) / length(rainfall_z)
proportion_z

###(c)
G <- function(x){
  p <- FR$precip[[x]]
  t <- which(p == max(p))
  t
  date <- FR$time[[x]][[t]]
  date
}
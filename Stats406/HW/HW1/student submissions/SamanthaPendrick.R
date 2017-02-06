#Stats 406 Homework 1
#Samantha Pendrick

#Problem 1
  #part a
  U1 = runif(1000,0,1)
  U2 = runif(1000,0,1)

  vecZ = sqrt(-2*log(U1))*cos(2*pi*U2)

  #part b
  #a
  mean(vecZ < 0)
  #b
  mean( (vecZ > 0) & (vecZ < 1) )
  #c
  mean(vecZ > 1 )

  
#Problem 2
  vec = 1:20
  Bn = abs(sin((2*pi*vec)/20))*choose(20,vec)*(.2)^(vec)*(1-.2)^(20-vec)
  Bnx = sum(Bn)
  #or
  vec = 1:20
  x = .2
  n = 20
  Bn2 = abs(sin((2*pi*vec)/n))*choose(n,vec)*x^(vec)*(1-x)^(n-(vec))
  Bnx2 = sum(Bn2)
  
#Problem 3
  #a
  datap3 = load('FrontRange.RData')
  info = FR$info
  high = which.max(info[[3]])
  #station 36 has the highest elevation
  low = which.min(info[[3]])
  #station 27 has the lowest elevation
  plot(FR$precip[[36]], main = "Highest Elevation", xlab = "Day", ylab = "precipitation")
  plot(FR$precip[[27]], main = "Lowest Elevation", xlab = "Day", ylab = "precipitation")
  
  #b
  mean((FR$precip[[36]]) >= 10)
  mean((FR$precip[[27]]) >= 10)
  
  #c
  highest_index = which.max(FR$precip[[10]])
  time = FR$time[[10]][highest_index]
  time
  
  
#Problem 1
#Part a
u1 = runif(n = 1000)
u2 = runif(n = 1000)
vecZ = cos(2*pi*u1)*sqrt(-2*log(u2))
#Part b
mean(vecZ < 0)
mean(vecZ > 0 & vecZ < 1)
mean(vecZ > 1)

#Problem 2
B = function(n,x) {
  vecB = seq(from = 0, to = n);
  sum(abs(sin((2*pi*vecB)/n))*choose(n, vecB)*(x^vecB)*(1-x)^(n-vecB));
}
B(20, .2)

#Problem 3
#Part a
load(FrontRange.RData)
frmax=match(max(FR$info$elev), FR$info$elev)
frmin=match(min(FR$info$elev), FR$info$elev)
plot(FR$time[[frmax]], FR$precip[[frmax]], type = 'l')
plot(FR$time[[frmin]], FR$precip[[frmin]], type = 'l')
#Part b
mean(FR$precip[[frmax]] >= 10)
mean(FR$precip[[frmin]] >= 10)
#Part c
maxPrecip = function(station) {
  FR$precip[[station]]
  maxdate=match(max(FR$precip[[station]]), FR$precip[[station]])
  FR$time[[station]][[maxdate]]
}
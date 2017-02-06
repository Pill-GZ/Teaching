#Problem 1 Part A
U1<-runif(n=1000,min=0,max=1)
U2<-runif(n=1000,min=0,max=1)
Z=(-2*log(U1))^(1/2)*cos(2*pi*U2)
vecZ=Z

#Problem 1 Part B
BelowZero=mean(vecZ<0)
BelowZero
BetweenZeroandOne=mean(vecZ>0 & vecZ<1)
BetweenZeroandOne
AboveOne=mean(vecZ>1)
AboveOne
#BelowZero+BetweenZeroandOne+AboveOne == 1

#Problem 2
B<-function(x,n){
  k=c(0:n)
  A=abs(sin((2*pi*k)/n))
  B=choose(n,k)
  C=(x)^k
  D=(1-x)^(n-k)
  E=A*B*C*D
  return(E)
}
Answer=sum(B(0.2,20))
Answer
#Answer is equal to 0.8099159

#Problem 3 Part A
load("~/FrontRange.RData")
#loads Data into R Global Environment
colMax <- function(data) sapply(data, max, na.rm = TRUE)
colMax(FR$info)
#highest elevation is 10260 which matches station 36
colMin <- function(data) sapply(data, min, na.rm = TRUE)
colMin(FR$info)
#lowest elevation is 4650, which matches station 27
DailyRainfallLowStation=FR$precip[[27]]
DailyRainfallHighStation=FR$precip[[36]]
DatesofRainLowStation=FR$time[[27]]
DatesofRainHighStation=FR$time[[36]]
plot(DatesofRainLowStation,DailyRainfallLowStation)
plot(DatesofRainHighStation,DailyRainfallHighStation)

#Problem 3 Part B
Lowstatprop=mean(DailyRainfallLowStation==10|DailyRainfallLowStation>10)
Lowstatprop
#Answer is equal to 0.1132226
Highstatprop=mean(DailyRainfallHighStation==10|DailyRainfallHighStation>10)
Highstatprop
#Answer is equal to 0.262334

#Problem 3 Part C
stationnum<-function(n){
  StationDateMax <- function(data) sapply(data, max, na.rm = TRUE)
  Highrainfall=StationDateMax(FR$precip[n])
  indexnum=match(Highrainfall,FR$precip[[n]])
  dateofhighrain=FR$time[[n]][indexnum]
  dateofhighrain
}
#Gives date of highest rainfall for selected station
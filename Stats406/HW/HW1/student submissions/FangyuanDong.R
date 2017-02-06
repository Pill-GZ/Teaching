#QUESTION1 a)
U1=runif(1000,0,1)
U2=runif(1000,0,1)
vecZ=sqrt(-2*log(U1))*cos(2*pi*U2)

#QUESTION b)
vecz1=vecZ[vecZ < 0]
vecz2=vecZ[vecZ > 0 & vecZ< 1]
vecz3=vecZ[vecZ > 1]
p1=length(vecz1)/length(vecZ)*100
p2=length(vecz2)/length(vecZ)*100
p3=length(vecz3)/length(vecZ)*100
paste(p1,"%", sep="")
paste(p2,"%", sep="")
paste(p3,"%", sep="")

#QUESTION2
B=function(x,n) {
  k=0:n
  Y=abs(sin(2*pi*k/n))*choose(n,k) *x^k * (1-x)^(n-k)
  X=sum(Y) 
  return(X)
}
B(0.2, 20) # B = 0.8099159

#QUESTION3
load('FrontRange.rdata')
#a)
which(FR$info$elev==max(FR$info$elev))
which(FR$info$elev==min(FR$info$elev))
##The station at the highest elevation is the 36th one;
##The Station at the lowest elevation is the 27th one;
rainfall1=FR$precip[[36]]
time1=FR$time[[36]]
rainfall2=FR$precip[[27]]
time2=FR$time[[27]]
plot(time1,rainfall1,xlab="Time",ylab="Rainfall Amounts",main="Highest Elevation Station Rainfalls overtime")
plot(time2,rainfall2,xlab="Time",ylab="Rainfall Amounts",main="Lowest Elevation Station Rainfalls overtime")
#b)
rain1=FR$precip[[36]][FR$precip[[36]]>=10]
rain2=FR$precip[[27]][FR$precip[[27]]>=10]
p4=length(rain1)/length(FR$precip[[36]]) 
p4 ##p4=0.262334
p5=length(rain2)/length(FR$precip[[27]])
p5 ##p5=0.1132226
#C)
station=function(x) {
  maxrain=which(FR$precip[[x]]==max(FR$precip[[x]]))
  date=FR$time[[x]][maxrain]
  return(date)
}
station(10)
#the date of the record highest amount of precipitation of station 10 is 1999.592

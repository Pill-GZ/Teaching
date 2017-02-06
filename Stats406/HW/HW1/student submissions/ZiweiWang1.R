#Q1
#(a)
vectorlength <- 1000;
U1 <- runif(vectorlength, min = 0, max =1);
U2 <- runif(vectorlength, min = 0, max =1);
vecZ <- sqrt(-2*log(U1))*cos(2*pi*U2);

#(b)
#below 0
vecbelowzero <- vecZ[vecZ<0];
vecbelowzeroportion <- length(vecbelowzero)/vectorlength;vecbelowzeroportion 

#between 0 and 1
vecpostive <- vecZ[vecZ>0];
vecMiddle <- vecpostive[vecpostive<1];
vecMiddleportion <- length(vecMiddle)/vectorlength;vecMiddleportion

#above 1
vecaboveone <- vecZ[vecZ>1];
vecaboveoneportion <- length(vecaboveone)/vectorlength;vecaboveoneportion

#Q2
n <- 20;
k <- 0:n;k
x <- 0.2
vectB <- abs(sin((2*pi*k)/n))*choose(n,k)*(x^k)*((1-x)^(n-k));vectB
sum <- sum(vectB);sum

#Q3
   

load("FrontRange.RData");

#(a)
#create a data frame
FRinfo <-FR$info; 
FRlocEle <- FRinfo$elev;FRlocEle
# Find the station at the highest elevation
maxEle <- max(FRlocEle);maxEle
maxEleindex <- which(FRlocEle == maxEle);maxEleindex
#36th

# Find the station at the lowest elevation
minEle <- min(FRlocEle);minEle
minEleindex<- which(FRlocEle == minEle);minEleindex
#27th

#plot the rainfall for 36th station
maxEleRain <-FR$precip[[maxEleindex]];
plot(maxEleRain,type = 'l', ylab = 'daily rainfalls', xlab = 'day',
     main = 'daily rainfalls for highest elevation')

#plot the rainfall for 27th station
minEleRain <-FR$precip[[minEleindex]];
plot(minEleRain,type = 'l', ylab = 'daily rainfalls', xlab = 'day',
     main = 'daily rainfalls for lowest elevation')

#(b)

#for highest elevation
maxEleOver10 <- maxEleRain[maxEleRain>=10];
maxEleOver10Portion <- length(maxEleOver10) /length(maxEleRain);maxEleOver10Portion

#for lowest elevation
minEleOver10 <- maxEleRain[minEleRain>=10];
minEleOver10Portion <- length(minEleOver10) /length(minEleRain);minEleOver10Portion

#(c)
#Write code to find for a given station (say station 10), the date of
#the record highest amount of precipitation

HighestPreDate <- function(x){

  maxRain <- max(FR$precip[[x]])
  maxRainIndex <- which(FR$precip[[x]] == maxRain)
  date <-FR$time[[x]][maxRainIndex]
  return (date)
}

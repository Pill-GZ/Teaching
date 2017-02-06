#************************************************************************************
#
#                     Homework 1      9/16/2016     Alan Longworth
#
#************************************************************************************

#load in Data
load("FrontRange.RData")
ls()
summary(FR)
summary(FR$precip$st050183)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Question 1
U1 = runif(n = 1000, min = 0, max = 1)
U2 = runif(n = 1000, min = 0, max = 1)

#a
vecZ = sqrt(-2*log(U1)) * cos(2*pi*U2)
vecZ[1:10]
summary(vecZ)
hist(vecZ)
#answer:
vecZ  #Looks similar to rnorm(u = 0, sd = 1)

#b
below = mean( vecZ<0 )
between = mean( vecZ>0 & vecZ<1 )
above = mean(vecZ>1)
#Answers:
below    #0.49
between  #0.348
above    #0.162



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Question 2
#n >= 1
#x in [0,1]

B20 = function(x){
  sum( 
    abs(sin(2*pi*(1:20)/20)) * choose(20, 1:20)*x^(1:20)*(1-x)^(20-1:20)
  )
}

#Answer:
B20(x = .2)   #Bn(x=.2)  =  0.8099159



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Question 3
#"precip" and "times" are lists, "info" is a data frame, and "Stot" is a vector

#a
#highest and lowest elevation
#of 56 stations
which.max(FR$info$elev)
which.min(FR$info$elev)
FR$info[36,]
FR$info[27,]

par(mfrow = c(1,2))
#Answers:
#Highest elevation: Station 36(st054750)    
#Lowest  elevation: Station 27(st053546)
plot(x = FR$time[[36]], y = FR$precip[[36]])
plot(x = FR$time[[27]], y = FR$precip[[27]])
  #they started rounding measurements to the nearest 10mm



#b
#what is the proportion of days with 10mm of rain or more?
mean(FR$precip[[36]] >= 10)
mean(FR$precip[[27]] >= 10)


#c
#the date of the record highest amount of precipitation.
which.max(FR$precip[[10]])
FR$time[[10]][5929]
#Answer: 1999.592




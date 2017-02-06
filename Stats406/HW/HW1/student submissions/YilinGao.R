#STATS 406
#HW1
#Yilin Gao

##############################################################################################
#Q1
#(a)
U1 = runif(1000,0,1) #generate 1000 uniform random variables for U1
U2 = runif(1000,0,1) #generate 1000 uniform random variables for U2
vecZ = sqrt(-2*log(U1))*(cos(2*pi*U2)) #apply the equation to generate 1000 random varables

#(b)
#(1)
mean(vecZ < 0) #the proportion of the elements of vecZ that fall below 0

#(2)
mean((vecZ > 0)&(vecZ < 1)) #the proportion of the elements of vecZ that fall between 0 and 1

#(3)
mean(vecZ > 1) #the proportion of the elements of vecZ that fall above 1

##############################################################################################
#Q2
n = 20 #assign value to n
x = 0.2 #assign value to x
k = 0:20 #k is a list of interger from 0 to 20
B = abs(sin(2*pi*k/n))*choose(n,k)*(x^k)*(1-x)^(n-k) #function of Bn
sum(B) #value of Bn. Bn = 0.8099159

##############################################################################################
#Q3
#(a)
load("~/Downloads/FrontRange.RData")  #load file
highelev = which(FR$info$elev == max(FR$info$elev)) #find the station at the highest elevation
lowelev = which(FR$info$elev == min(FR$info$elev))  #find the station at the lowest elevation
plot(FR$time[[highelev]],FR$precip[[highelev]],xlab="Time",ylab="Daily rainfallS") 
#plot the daily rainfalls over time at highest elevation station
plot(FR$time[[lowelev]],FR$precip[[lowelev]],xlab="Time",ylab="Daily rainfallS")  
#plot the daily rainfalls over time at lowest elevation station

#(b)
mean(FR$precip[[highelev]]>=10) 
#proportion of days with 10mm of rain or more at the highest elevation station
#answer is 0.262334
mean(FR$precip[[lowelev]]>=10)
#proportion of days with 10mm of rain or more at the loweset elevation station
#answer is 0.1132226

#(c)
FR$time[[10]][which(FR$precip[[10]] == max(FR$precip[[10]]))]  
#the date of the record highest amount of precipitation at given station 10
#answer is 1999.592
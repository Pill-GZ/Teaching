#Name: Biyun Jiang
#Stats 406
#HW 1

#1(a) 
U1 = runif(1000, min = 0, max = 1) #Identify U1 as a r.v with U(0,1)
U2 = runif(1000, min = 0, max = 1) #Identify U2 as a r.v with U(0,1)
vecZ = numeric(1000) #a vector of length n = 1000
vecZ = sqrt(-2*log(U1))*cos(2*pi*U2) #each component of the vector is a r.v with the same distribution as Z.

#1(b)
#(a)
mean(vecZ < 0) #the proportion of the elements of vecZ that fall below 0
#(b)
mean(vecZ>0 & vecZ<1) #the proportion of the elements of vecZ that fall between 0 and 1
#(c)
mean(vecZ>1) #the proportion of the elements of vecZ that fall above 1

#2
n = 20 #Assign value to variable n
x = 0.2 #Assign value to variable x
k = 0:20 #Assign values to variable k
B = abs(sin(2*pi*k/n))*choose(n,k)*(x^k)*(1-x)^(n-k) #Create vector B
Bn = sum(B) # Calculate Bn(0.2)

#3(a)
load("D:/2016 Fall/Stats 406/Homework/Stat 406 HW/FrontRange.RData")
highEle = which.max(FR$info[[3]]) #Find the station at the highest elevation
lowEle = which.min(FR$info[[3]]) #Find the station at the lowest elevation
plot(FR$time[[highEle]], FR$precip[[highEle]], xlab = 'Time', ylab = 'Daily Rainfalls') #Plot the station with highest elevation
plot(FR$time[[lowEle]], FR$precip[[lowEle]], xlab = 'Time', ylab = 'Daily Rainfalls') #Plot the station with lowest elevation

#3(b)
mean(FR$precip[[highEle]] >= 10) #Highest station's proportion of days precip >= 10
mean(FR$precip[[lowEle]] >= 10) #Lowest station's proportion of days precip >= 10

#3(c)
highPrecip10 = which.max(FR$precip[[10]]) #Find station 10's highest precip
date10 = FR$time[[10]][[highPrecip10]] # Date of station 10's highest precip




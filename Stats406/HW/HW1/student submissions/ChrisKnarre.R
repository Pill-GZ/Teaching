load(FrontRange.RData)

#problem 1
#a)
vecZ = (sqrt(-2*log10(runif(1000, 0, 1))))*cos(2*pi*runif(1000, 0, 1))
vecZ
#b) i)
vecZltz = vecZ[vecZ < 0]
vecZltz #num of entries < 0
propltz = length(vecZltz)/1000
propltz #proportion of entries < zero
#ii)
vecZbzo = vecZ[vecZ >= 0 & vecZ <= 1]
vecZbzo #num of entries >= 0 and <= 1
propbzo = length(vecZbzo)/1000
propbzo #proportion of entries >= 0 and <= 1
#iii)
vecZgto = vecZ[vecZ > 1]
vecZgto #num of entries > 1
propgto = length(vecZgto)/1000
propgto #proportion of entries > 1


#problem 2
Bn = sum(abs(sin((2*pi*(0:20))/20))*choose(20,(0:20))*(.2^(0:20))*((.8)^(20-(0:20))))
Bn #result

#problem 3
#a)
max = which(FR$info$elev == max(FR$info$elev))
max #highest elev station
y = unlist(FR$precip[max])
x = unlist(FR$time[max])
plot(x,y) #highest elev plot
min = which(FR$info$elev == min(FR$info$elev))
min #lowest elev station
y = unlist(FR$precip[min])
x = unlist(FR$time[min])
plot(x,y) #lowest elev plot
#b)
x = unlist(FR$precip[max])
length(x[x>=10])/length(x) #proportion of days in max elev above 10mm
y = unlist(FR$precip[min])
length(y[y>=10])/length(y) #proportion of days in min elev above 10mm
#c)
input = 10 #in this case
entry = which(unlist(FR$precip[input]) == max(unlist(FR$precip[input])))
#finds max rainfall's position
unlist(FR$time[input])[entry] #time of highest precipitation for input
#Matthew Stieg
#Stats 406
#Homework 1
#Due Sep 22

# 1

U1 = runif(1000,0,1)
U2 = runif(1000,0,1)
Z = sqrt(-2*log(U1))*(cos(2*pi*U2))
Below0 = Z[Z<0]; length(Below0)
Btwn01 = Z[0<Z&Z<1]; length(Btwn01)
Above1 = Z[Z>1]; length(Above1)

#2

x = 0.2
n = 20
k = 1:20
Bn = abs(sin(2*pi*k/n))*(choose(n,k))*(x^k)*((1-x)^(n-k))
Bn = sum(Bn)

#3

load('FrontRange.RData')
High = which.max(FR$info$elev)
Low = which.min(FR$info$elev)
High_rain = FR$precip[[High]]
Low_rain = FR$precip[[Low]]
plot(1:length(Low_rain),Low_rain)
plot(1:length(High_rain),High_rain)

#b)
#porportion of heavy rain days for High & Low Stations
p_high = length(High_rain[High_rain>=10])/length(High_rain); p_high
p_low = length(Low_rain[Low_rain>=10])/length(Low_rain); p_low

#c)

station = 10
high_date = which.max(FR$precip[[station]]); high_date

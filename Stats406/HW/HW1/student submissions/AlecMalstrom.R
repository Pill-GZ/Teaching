# Problem 1
U1 = runif(n=1000, min=0, max=1)
U2 = runif(n=1000, min=0, max=1)
vecZ=sqrt(-2*log(U1))*cos(2*pi*U2)

print(sum(vecZ<0)/1000)
print(sum((vecZ>=0)&(vecZ<1))/1000)
print(sum(vecZ>=1)/1000)

# Problem 2
n_list = 0:20
Bnx = sum(abs(sin(2*pi*n_list/20))*choose(n=20, k=n_list)*(0.2^n_list)*(1-0.2)^(20-n_list))
print(Bnx)

# Problem 3
load('FrontRange.RData')

min_el_ind = which.min(FR$info$elev)
max_el_ind = which.max(FR$info$elev)
print(min_el_ind)
print(max_el_ind)
plot(FR$precip[[min_el_ind]])
plot(FR$precip[[max_el_ind]])

print(sum(FR$precip[[min_el_ind]]>=10)/length(FR$precip[[min_el_ind]]))
print(sum(FR$precip[[max_el_ind]]>=10)/length(FR$precip[[max_el_ind]]))

given_station = 10
date_ind = which.max(FR$precip[[given_station]])
print(FR$time[[given_station]][date_ind])

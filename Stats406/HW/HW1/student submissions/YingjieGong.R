#######################################

#1

#(a)
u1 = runif (n=1000,min=0,max=1);
u2 = runif (n=1000,min=0,max=1);
vecZ = (sqrt(-2*log(u1)))*(cos(2*pi*u2));

#(b)-(a)
index_sel1 = which(vecZ<0);
length(index_sel1)/length(vecZ);
#(b)-(b)
index_sel2 = which((vecZ>0) & (vecZ<1));
length(index_sel2)/length(vecZ);
#(b)-(c)
index_sel3 = which(vecZ>1);
length(index_sel3)/length(vecZ);

#######################################

#2
k = c(0:20);
sum((abs(sin(0.1*pi*k)))*(choose(20,k))*(0.2^k)*(0.8^(20-k)));

#######################################

#3
load('FrontRange.RData');

#(a)
aa = FR$info;
names(aa);
maxelevstation = which.max(aa$elev);
maxelevstation;
plot(FR$time[[maxelevstation]], FR$precip[[maxelevstation]], xlab='date', ylab='daily rainfall', type='l', col="blue");
minelevstation = which.min(aa$elev);
minelevstation;
plot(FR$time[[minelevstation]],FR$precip[[minelevstation]], xlab='date', ylab='daily rainfall', type='l', col='red');

#(b)
index_sel1 = which(FR$precip[[maxelevstation]]>=10);
length(index_sel1)/length(FR$precip[[maxelevstation]]);
index_sel2 = which(FR$precip[[minelevstation]]>=10);
length(index_sel2)/length(FR$precip[[minelevstation]]);

#(c) suppose the given station is 10
maxprepindex = which.max(FR$precip[[10]]);
maxprepindex;
FR$time[[10]][maxprepindex]

#######################################
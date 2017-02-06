##1(a)
U1=runif(1000,min=0,max=1);
U2=runif(1000,min=0,max=1);
vecZ=sqrt(-2*log(U1))*cos(2*pi*U2);
##1(b)(a)
mean(vecZ<0);
##1(b)(b)
mean(vecZ>=0&vecZ<=1);
##1(b)(c)
mean(vecZ>1);

##2
k = 1:20;
b=abs(sin(2*pi*k/20))*choose(20,k)*0.2^k*(1-0.2)^(20-k);
Bn=sum(b);

##3(a)
elev = FR$info[[3]];
hs = which(elev == max(elev));
ls = which(elev == min(elev));
htime = FR$time[[hs]];
hprecip = FR$precip[[hs]];
ltime = FR$time[[ls]];
lprecip = FR$precip[[ls]];
plot(htime, hprecip);
plot(ltime, lprecip);
##3(b)
hprop = mean(hprecip>=10);
lprop = mean(lprecip>=10);
##3(c)
f = function(n){
	precip = FR$precip[[n]];
	dates = FR$time[[n]];
	date = dates[which(precip == max(precip))];
	return(date)
}
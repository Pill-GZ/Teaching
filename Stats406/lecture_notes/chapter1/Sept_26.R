dt_UN = read.delim('UNData.tab',header=T,quote="")
##
#Spectrum

dt_spec = read.delim('spectrum.csv',header = F,skip = 26);
dt_spec = read.csv('spectrum.csv',header =F, skip=26,sep= '\t')

### Traffic flow
dt_traffic = read.csv('traffic_flow.txt')
## Earmarks

dt_cong = read.delim('earmarks.txt',header=T)

##Plots
dt = airquality
boxplot(dt$Temp)
plot(dt$Temp,type='l')
plot(dt$Temp,dt$Wind,type='p',xlab='Temperature',ylab = 'Wind',main='Wind vs Temp')

par(mfrow = c(1,2))
boxplot(dt$Temp,main='Boxplot');
plot(dt$Temp,type='l',main ='Temperature')

m = matrix(c(1,2),ncol=2)
layout(m)
boxplot(dt$Temp,main='Boxplot');
plot(dt$Temp,type='l',main ='Temperature')

##
m = matrix(c(1,3,2,3),ncol=2)
layout(m)
boxplot(dt$Temp,main='Boxplot');
plot(dt$Temp,type='l',main ='Temperature')
plot(dt$Temp, dt$Wind,type='p',main = 'xyplot')

X = rnorm(n=10000);
hist(X,breaks=200,prob=T,col = 'blue', xlim=c(-4,4), ylim=c(0,0.4))
par(new=T)
curve(dnorm,lwd=2,xlab='',ylab='',col='red',xlim=c(-4,4),ylim=c(0,0.4))

###Legend example
dens = density(X);
plot(dens, col = 'blue', lty = 2, xlim=c(-4,4), ylim=c(0,0.4))
par(new=T)
curve(dnorm,lty=1, col='red',xlim=c(-4,4),ylim=c(0,0.4))
legend(x='topleft',legend=c('Estimated density','True density'),col=c('blue','red'),lty=c(2,1))









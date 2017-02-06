############## Q1. Generate random variables ###############

# 1a
n = 5000
lambda = 3
x = -(1/lambda)*log(runif(n))
y = seq(0,10,length = 1000)
#png("1a.png")
hist(x, prob = TRUE, main = 'Histogram of Exp(3)')
lines(y,dexp(y,3))
#dev.off()

# 1b 
n = 10
k = 5
lambda = 3
x = matrix(-(1/lambda)*log(runif(n*k)), ncol=k)
g = apply(x, 1, sum)


#################### Q2. Rejection sampling I ##################

RejBeta1 = function(n, alpha, beta){
	Vy = numeric(n)
	Vcpt = integer(n)
	j=1;cpt=0;
	while(j <=n ){
		u = runif(1); y = runif(1); cpt =cpt + 1
		if(u <= y^(alpha-1)*(1-y)^(beta - 1)){
			Vy[j] = y; Vcpt[j] = cpt
			j=j+1; cpt=0
		}
	}
	return(list(Vy,Vcpt))
}

betarnd = RejBeta1(10, 2, 2)


###################### Q3. Empirical CDF ##################

Phi.m = function(m,xseq){
	# xseq can be either a scalar or a vector
	X = rnorm(m)
	Phi.m.x = sapply(xseq,function(x){mean(X<x)})
	return(Phi.m.x)
}

m = c(5,50,500)
xseq = seq(-4,4,by=0.01)
plot(0,xlim=c(-4,4),ylim=c(0,1),xlab="x",ylab="Phi.m(x)",main="Empirical CDF", type='n')
for (i in 1:length(m)){
	par(new=T)
	plot(xseq,Phi.m(m[i],xseq),col=i,axes=F,type="l",xlim=c(-4,4),ylim=c(0,1),xlab="",ylab="")
}
lines(xseq,pnorm(xseq,mean=0,sd=1),col="blue",lty=3,lwd=2)
legend("topleft",legend=paste(m,'points'),col=1:length(m),lty=1)


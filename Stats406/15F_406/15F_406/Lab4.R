## Q1. Generate random variables

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

# 1c
n = c(10, 20, 50, 100, 200, 1000)
g = list()
k = 5
lambda = 3
g = lapply(n, function(i) matrix(-(1/lambda)*log(runif(i*k)), ncol=k))
class(g) #to see what is the data structure used for g
sapply(g, dim) #check the dimension of each matrix in g
## the following will get the gamma random numbers of different sizes n. ##
gamma = lapply(g, function(M) apply(M,1,sum)) 
class(gamma) #to see what is the data structure
sapply(gamma, length) #check the dimension of each vector in g, correct number of gamma random variables generated

# 1d
#continue with gamma from the output of part (c)
grid = 1000
y = seq(0,10,length = grid)
plot(0,xlim=c(0,10),ylim=c(0,1),xlab="x",ylab="Density",main="Gamma density estimation", type='n')
#png('1d23.png')
par(mfrow=c(2,3))
for(i in 1:6){
    di = density(gamma[[i]],from=0,to=10, n = grid, bw=0.2)
    plot(di, xlim=c(0,10), ylim=c(0,1), axes=F, col=rainbow(6)[i], lwd = 2, main="Gamma density estimation")
    lines(y,dgamma(y,5,3), col = 'orange', lty = 3, lwd = 2)
}
#dev.off()
##To summarize multiple plot : http://cran.r-project.org/doc/contrib/Lemon-kickstart/kr_addat.html



## Q2. Empirical CDF

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
setwd('/Users/guojun/Google Drive/15Fall/homework/lab7')
## Example 1: choice of trial distribution g(x) ##

# part a).

h = function(x) (cos(x/3)^2 * x^2)
set.seed(20)
ISa = function(lambda, n){
	u = runif(n);
	x = -log(u)/lambda;
	w = exp(-(1-lambda)*x)
	wbar = mean(w)
	muhat = sum(w * h(x)) / sum(w)
	
	sample = w * h(x) / wbar
	#compm = mean(sample) # should equal to muhat.
	se = sqrt(var(sample)/n)
	CI0 = muhat - qnorm(0.975) * se
	CI1 = muhat + qnorm(0.975) * se
	return(c(muhat, se, CI0, CI1))
}

n = 10000
isa.5 = ISa(0.5, n)
isa1 = ISa(1, n)



# part b).

ISb = function(n){
	x = rgamma(n, shape = 3, rate = 1)
	w = x^{-2}
	wbar = mean(w)
	muhat = sum(w * h(x)) / sum(w)
	
	sample = w * h(x) / wbar
	#compm = mean(sample) # should equal to muhat.
	se = sqrt(var(sample)/n)
	CI0 = muhat - qnorm(0.975) * se
	CI1 = muhat + qnorm(0.975) * se
	return(c(muhat, se, CI0, CI1))
}

n = 10000
isb = ISb(n)

# part c

xseq = seq(0, 10, by = 0.2);
y = h(xseq) * exp(-xseq);
pdf('compare_02.pdf')
par(mfrow = c(2,2))
plot(xseq, y, 'l', lwd=1.5, ylim = c(0,1), main=expression(paste(g(x),'=',e^{-x})))
#text(2.2, 0.4, expression(paste(abs(h(x)),pi(x))))
lines(xseq, dexp(xseq, rate = 1), lty = 2, lwd=1.5, col = 'blue')
legend('topright', legend = c(expression(paste(abs(h(x)),pi(x))),expression(g(x) == e^{-x})), col = c('black','blue'), lty = c(1, 2),lwd=c(1.5,1.5),text.col=c('black','blue'))
#text(0.65, 0.8, expression(g(x) == e^{-x}),col='blue')

plot(xseq, y, 'l', lwd=1.5, ylim = c(0,.5), main=expression(g(x) == 0.5*e^{-0.5*x}))
lines(xseq, dexp(xseq, rate = 0.5), lty = 2, lwd=1.5, col = 'blue')
legend('topright', legend = c(expression(paste(abs(h(x)),pi(x))),expression(g(x) == 0.5*e^{-0.5*x})), col = c('black','blue'), lty = c(1, 2), lwd=c(1.5,1.5), text.col=c('black','blue'))

plot(xseq, y, 'l', lwd=1.5, main=expression(g(x) == 0.5*x^2*e^{-x}))
lines(xseq, dgamma(xseq, shape = 3, rate = 1), lty = 2, lwd=1.5, col = 'blue')
legend('topright', legend = c(expression(paste(abs(h(x)),pi(x))),expression(g(x) == 0.5*x^2*e^{-x})), col = c('black','blue'), lty = c(1, 2),lwd=c(1.5,1.5),text.col=c('black','blue'))

plot(xseq, y, 'l', lwd=1.5, ylim = c(0,1), main = "g(x) altogether")
lines(xseq, dexp(xseq, rate = 1), lty = 2, lwd=1.5, col = 'green')
lines(xseq, dexp(xseq, rate = 0.5), lty = 2, lwd=1.5, col = 'red')
lines(xseq, dgamma(xseq, shape = 3, rate = 1), lty = 2, lwd=1.5, col = 'blue')
legend('topright', legend = c(expression(paste(abs(h(x)),pi(x))),expression(g(x) == e^{-x}),expression(g(x) == 0.5*e^{-0.5*x}),expression(g(x) == 0.5*x^2*e^{-x})), col = c('black','green','red','blue'), lty = c(1, 2,2,2),lwd=c(1.5,1.5,1.5,1.5),text.col=c('black','green','red','blue'))
dev.off()



## Example 2: target density has an unknown constant, use weighted average IS ##

# n is the number of monte carlo samples,
# lambda is the rate parameter for the g (exponential) distribution

IS = function(input){
	lambda = input[1]
	n = input[2]
	
	# Target density with unknow constant C
	f = function(t) (exp(-t) / (t+1))
	
	# trial density, g
	g = function(t) dexp(t, lambda)
	
	# importance function
	w = function(t) f(t) / g(t)
	
	# draw sample from g
	X = rexp(n, lambda)
	
	# calculate the list of importance values
	W = w(X)
	
	# importance sampling estimate
	I = mean( X * W ) / mean(W)
	
	# calculate sample coefficient of variation CV
	CV = sqrt( var( W / mean(W) ) )
	
	# calculate importance sampling error
	sig.sq = var( X * W / mean(W) )
	se = sqrt( sig.sq / n )
	
	output = c(I, se, CV)
	return(output)

}

## calculate CV for a grid of values of lambda
lambda.val <- seq(.02, 3, length=100)
n.val <- rep(1000, 100)
# inpt.mat is a 500 times 2 matrix containing all the inputs, each row of inpt.mat is an input
inpt.mat <- matrix(c(lambda.val, n.val), nrow = 100, ncol = 2)

## apply the weighted average Importance Sampling function IS() to every row in inpt.mat
A <- apply(inpt.mat,1, IS) # each output of IS() function as c(I, se, CV) is put as a column of A
A <- t(A) # transpose A to get each row of A as one output.


# see where CV is low enough
pdf('lowCV.pdf')
plot(lambda.val, A[,3], ylab="CV", xlab="Lambda", main="CV vs. lambda", col=2, type="l")
abline(h=5) # only those with a CV value below 5 are considered stable
abline(v = lambda.val[which.min(A[,3])], col = 'blue')
text(1.5, 4, "lambda = 1.4949, minCV = 0.09, SE = 0.03")
dev.off()

# importance sampling error estimates (standard errors of IS estimates)
pdf('lowSE.pdf')
plot(lambda.val, A[,2], xlab="Lambda", ylab="standard error", main = "Standard error vs. Lambda", col=4, type="l")
abline(v = lambda.val[which.min(A[,2])], col = 'red')
text(1, 0.1, "lambda = 0.71,minSE = 0.0077, CV = 0.61")
dev.off()

# final answer : the one exp(lambda) denstiy resulting in the smallest CV and its corresponding IS() outputs c(I, se, CV). You can see its estimation error se is also small among other choices of lambda.
indx = which.min(A[,3])
fin.ans <- c(lambda.val[indx], A[indx,])
indx_se <- which.min(A[,2])
ans.mse <- c(lambda.val[indx_se], A[indx_se,])

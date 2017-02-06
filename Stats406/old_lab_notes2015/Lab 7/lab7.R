## Example 1: choice of trial distribution g(x) ##

# part a).

h = function(x) (cos(x/3)^2 * x^2)

ISa = function(lambda, n){
	u = runif(n);
	x = -log(u)/lambda;
	w = exp(-(1-lambda)*x)
	muhat = sum(w * h(x)) / sum(w)
	
	
	return(muhat)
}

n = 10000
isa.5 = ISa(0.5, n)
isa1 = ISa(1, n)

# part b).

ISb = function(n){
	x = rgamma(n, shape = 3, rate = 1)
	w = x^{-2}
	muhat = sum(w * h(x)) / sum(w)
	
	
	return(muhat)
}

n = 10000
isb = ISb(n)

## Example 2: target density has an unknown constant, use weighted average IS ##

# n_mc is the number of monte carlo samples,
# lambda is the rate parameter for the g (exponential) distribution

IS = function(input){
	lambda = input[1]
	n_mc = input[2]
	
	# Target density with unknow constant C
	f = function(t) (exp(-t) / (t+1))
	
	# trial density, g
	g = function(t) dexp(t, lambda)
	
	# importance function
	w = function(t) f(t) / g(t)
	
	# draw sample from g
	X = rexp(n_mc, lambda)
	
	# calculate the list of importance values
	LW = w(X)
	
	# importance sampling estimate
	I = mean( X * LW ) / mean(LW)
	
	# calculate sample coefficient of variation CV
	CV = sqrt( var( LW / mean(LW) ) )
	
	# calculate importance sampling error
	sig.sq = var( LW*X / mean(LW) )
	se = sqrt( sig.sq / n_mc )
	
	output = c(I, se, CV)
	return(output)

}

## calculate CV for a grid of values of lambda
lambda.val <- seq(.05, 10, length=500)
n_mc.val <- rep(1000, 500)
# inpt.mat is a 500 times 2 matrix containing all the inputs, each row of inpt.mat is an input
inpt.mat <- matrix(c(lambda.val, n_mc.val), nrow = 500, ncol = 2)
 ## apply the weighted average Importance Sampling function IS() to every row in inpt.mat
A <- apply(inpt.mat,1, IS) # each output of IS() function as c(I, se, CV) is put as a column of A
A <- t(A) # transpose A to get each row of A as one output.
 
# see where CV is low enough
plot(lambda.val, A[,3], ylab="CV", xlab="Lambda", main="CV vs. lambda", col=2, type="l")
abline(h=5) # only those with a CV value below 5 are considered stable
 
# importance sampling error estimates (standard errors of IS estimates)
plot(lambda.val, A[,2], xlab="Lambda", ylab="standard error vs. Lambda", col=4, type="l")
 
# final answer : the one exp(lambda) denstiy resulting in the smallest CV and its corresponding IS() outputs c(I, se, CV). You can see its estimation error se is also small among other choices of lambda.
indx = which.min(A[,3])
fin.ans <- c(lambda.val[indx], A[indx,])

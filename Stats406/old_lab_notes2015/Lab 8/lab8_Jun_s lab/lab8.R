setwd("/Users/guojun/Google Drive/15Fall/homework/lab8")

load("lab8.Rdata")
# what does the data look like
head(sampledata)

n = dim(sampledata)[1]
x = sampledata[,1]
y = sampledata[,2]

## calculate lambda-hat
lambda.hat = n/sum(x*y)

## specify bootstrap sample size
B = 50

## use parametric bootstrap for y (treat x as fixed input data, not a random variable - i.e. we don't model x)
lambda.hat.pboot = c() # save the estimated lambda.hat for each bootstrapped sample
for (i in 1:B){
	y.sample = c()
	for (j in 1:n){
		y.sample[j] = rexp(1,lambda.hat*x[j])
	}  
	lambda.hat.pboot[i] = n/sum(x*y.sample)
}
# bias
mean(lambda.hat.pboot)-lambda.hat
# variance
var(lambda.hat.pboot)
# MSE
mean((lambda.hat.pboot - lambda.hat)^2)




## use nonparametric bootstrap for (x, y), since (x, y) are given as tuples, whether we model or not model x makes no difference in drawing bootstrap samples for (x, y) by resampling from (x_i, y_i), i =1, ..., n. For this regard, refer to the algorithm and coding in the last lecture note example in nonparametric bootstrap too, which is estimating the correlation coefficient based on a sample data (x_i, y_i), i =1, ..., n.

lambda.hat.npboot = c() 
for (i in 1:B){
	sample.id = sample(1:n,size=n, replace=TRUE)
	lambda.hat.npboot[i] = n/sum(x[sample.id]*y[sample.id])
}
# bias
mean(lambda.hat.npboot)-lambda.hat
# variance
var(lambda.hat.npboot)
# MSE
mean((lambda.hat.npboot - lambda.hat)^2)




## A further discussion if you are interested : if we treat x input as realizations of random variable - i.e. we do model x, then to use parametric bootstrap for each y_i condition on x_i, i = 1, ..., n, we do need to use nonparametric bootstrap to sample for x (there's no way however to do parametric bootstrap to sample x since the distribution of x is entirely not specified in this problem).

lambda.hat.pnpboot = c() # save the estimated lambda.hat for each bootstrapped sample
for (i in 1:B){
	y.sample = c()
	xsample.id = sample(1:n,size=n, replace=TRUE)
	x.sample = x[xsample.id]
	for (j in 1:n){
		y.sample[j] = rexp(1,lambda.hat*x.sample[j])
	}  
	lambda.hat.pnpboot[i] = n/sum(x.sample*y.sample)
}
# bias
mean(lambda.hat.pnpboot)-lambda.hat
# variance
var(lambda.hat.pnpboot)
# MSE
mean((lambda.hat.pnpboot - lambda.hat)^2)




















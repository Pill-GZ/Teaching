
###############################################################
#Q1
###############################################################

## Number of replicates
num_replicate <- 1000
## Number of samples
n <- 50
## Different thetas
thetas <- seq(0.5, 10, by=0.1)
num_theta <- length(thetas)

## Initialize MSEs
MSE <- matrix(0, num_theta, 2)
## Initialize bias
bias <- matrix(0, num_theta, 2)

for (i in seq(1, num_theta))
{
    ## Generate the data for all replications, each row is a replication
    D <- matrix(rnorm(n*num_replicate, mean=thetas[i], sd=thetas[i]), n, num_replicate)

    ## Theta_hat estimted by mean
    Thetahat1 <- apply(D, 2, mean)

    ## Theta_hat estimted by standard deviation
    Thetahat2 <- apply(D, 2, sd)

    ## Record MSEs
    MSE[i, 1] <- mean((Thetahat1 - thetas[i])^2)
    MSE[i, 2] <- mean((Thetahat2 - thetas[i])^2)

    ## Record bias
    bias[i, 1] <- mean(Thetahat1 - thetas[i])
    bias[i, 2] <- mean(Thetahat2 - thetas[i])
}

## Plot MSEs
plot(thetas, MSE[, 1], xlab=quote(theta), ylab='MSE', type='l', col=1)
lines(thetas, MSE[, 2], lty=2, col=2)

## Plot bias
plot(thetas, bias[, 1], xlab=quote(theta), ylab='bias', type='l', col=1)
lines(thetas, bias[, 2], lty=2, col=2)
###############################################################
# Exercise
###############################################################
## Number of replicates
num_replicate <- 1000
## Number of samples
n <- 50
## Different thetas
thetas <- seq(0.5, 10, by=0.1)
num_theta <- length(thetas)

## Initialize MSEs
MSE <- matrix(0, num_theta, 2)
## Initialize bias
bias <- matrix(0, num_theta, 2)

for (i in seq(1, num_theta))
{
  ## Generate the data for all replications, each row is a replication
  D <- matrix(runif(n*num_replicate, 0, thetas[i]), n, num_replicate)
  
  ## Theta_hat estimted by mean
  Thetahat1 <- 2*apply(D, 2, mean)
  
  ## Theta_hat estimted by standard deviation
  Thetahat2 <- (n+1)/n*apply(D, 2, max)
  
  ## Record MSEs
  MSE[i, 1] <- mean((Thetahat1 - thetas[i])^2)
  MSE[i, 2] <- mean((Thetahat2 - thetas[i])^2)
  
  ## Record bias
  bias[i, 1] <- mean(Thetahat1 - thetas[i])
  bias[i, 2] <- mean(Thetahat2 - thetas[i])
}

## Plot MSEs
plot(thetas, MSE[, 1], xlab=quote(theta), ylab='MSE', type='l', col=1)
lines(thetas, MSE[, 2], lty=2, col=2)

## Plot bias
plot(thetas, bias[, 1], xlab=quote(theta), ylab='bias', type='l', col=1)
lines(thetas, bias[, 2], lty=2, col=2)

###############################################################
#Q2
###############################################################

n=20 #sample size
n_rep = 100 # no. of replications 

b_est <- function(mu){
Xbar = mean(rnorm(n, mu,1))
theta_hat = (Xbar)^2
B=500 #Number of Bootstrap replications
allb_rep = matrix(rnorm(n*B,mu,1), B,n)
theta_hatb = (apply(allb_rep,1, mean))^2
MSE = mean((theta_hatb - theta_hat)^2)
Bias = mean((theta_hatb - theta_hat))
return(c(Bias,MSE))
}

#Average over n_rep
ave_val <- function(mu, nrep) {
murep = matrix(mu, n_rep,1)
A = apply(murep,1,b_est)
return( apply(A,1, mean))
}

mu_val = as.matrix(seq(-10,10, length = 20),20,1)

A = apply(mu_val , 1 , ave_val)
par(mfrow = c(1,2))
plot(mu_val, A[1,], ylab="Bias_est", xlab="mu", main="Bias vs. mu", col=2, type="l")
plot(mu_val, A[2,], ylab="MSE_est", xlab="mu", main="MSE vs. mu", col=4, type="l")

###############################################################
# Exercise
###############################################################

n=20 #sample size
n_rep = 100 # no. of replications 

b_est <- function(mu){
  Xbar = mean(rnorm(n, mu,1))
  theta_hat = (Xbar)^2
  B=500 #Number of Bootstrap replications
  allb_rep = matrix(rnorm(n*B,mu,1), B,n)
  theta_hatb = abs(apply(allb_rep,1, mean))
  MSE = mean((theta_hatb - theta_hat)^2)
  Bias = mean((theta_hatb - theta_hat))
  return(c(Bias,MSE))
}

#Average over n_rep
ave_val <- function(mu, nrep) {
  murep = matrix(mu, n_rep,1)
  A = apply(murep,1,b_est)
  return( apply(A,1, mean))
}

mu_val = as.matrix(seq(-10,10, length = 20),20,1)

A = apply(mu_val , 1 , ave_val)
par(mfrow = c(1,2))
plot(mu_val, A[1,], ylab="Bias_est", xlab="mu", main="Bias vs. mu", col=2, type="l")
plot(mu_val, A[2,], ylab="MSE_est", xlab="mu", main="MSE vs. mu", col=4, type="l")

###############################################################
# Q2b
###############################################################

## Number of simulation replications.
nrep = 100
## Number of bootstrap samples.
nboot = 1000
## Population value of the upper limit of the uniform distribution.
a = 2
## Sample sizes.
SS = c(5,10,20)
## Storage for the bias estimates.
Bias = NULL
## Loop over the sample sizes.
for (k in 1:3) {
  ## The sample size for the current iteration.
  n = SS[k]
  bias = NULL
  for (r in 1:nrep) {
    ## Generate a sample from the uniform population.
    X = runif(n, max=a)
    ## Generate non-parametric bootstrap samples.
    ii = ceiling(n*runif(nboot*n))
    Xboot = X[ii]
    Xboot = array(Xboot, c(nboot, n))
    ## The bootstrap estimate of the relative bias.
    MX = apply(Xboot, 1, max)
    bias[r] = (mean(MX) - max(X))/max(X)
  }
  ## The overall estimate of the relative bias.
  Bias[k] = mean(bias)
}

###############################################################
#Q3
###############################################################
## Sample sizes.
N = c(10,20,40,60)
nrep = 1000 ## Number of simulation replications per sample size value.
nboot = 1000 ## The number of bootstrap data sets.
## Coverage probabilities.
CP = NULL
for (j in 1:length(N))
{
## Keep track of how many times the interval covers the true value.
nc = 0
n = N[j]
for (k in 1:nrep)
{
## Simulate a data set.
X = rnorm(n)
## Generate bootstrap data sets from X.
ii = ceiling(n*runif(n*nboot))
B = X[ii]
B = array(B, c(nboot,n))
## Get the sample mean for each bootstrap data set.
M = apply(B, 1, mean)
M = sort(M)
## Get the confidence interval lower and upper bound.
C = c(M[25], M[975])
## Check for coverage.
if ( (C[1] < 0) & (C[2] > 0) ) { nc = nc+1 }
}
CP[j] = nc/nrep
}






## Function to implement the inversion sampling
## for binomial distribution
binomial <- function(n, num_trials, prop)
{
  ## Generate n uninform numbers
  z <- runif(n)
  ## Obtain P(X=0), P(X=1), ..., P(X=num_trails)
  p <- choose(num_trials, seq(0, num_trials))* prop^seq(0, num_trials) * (1-prop)^(num_trials-seq(0, num_trials))
  ## Intialize the binomial numbers
  x <- rep(0, n)
  for (i in seq(1, n))
  {
    s <- 0  ## Initialize the sum
    k <- -1  ## Initialize
    ## While loop
    ## Finding the smallest k such that p[1]+...p[k] > z[i]
    while (s < z[i])
    {
      k <- k + 1
      s <- s + p[k+1]
    }
    x[i] <- k
  }
  
  return(x)
}

## Call binomial generator
x <- binomial(n=1000, num_trials=50, prop=0.2)
hist(x)



rm(list=ls(all=TRUE)) ## clear out old variables from the memory

## The input parameter is #samples and level of significance
integral <- function(n, alpha)
{
  u1 <- runif(n,min=-1, max =1)
  u2 <- runif(n,min=-1, max =1)
  h<- (abs(u1 + u2) <= 1)
  integral <- mean(h)
  mc_error <- sqrt(var(h) / n)
  z_alphaby2 <- qnorm(1- alpha/2)
  ci <- c( integral - z_alphaby2*mc_error, integral + z_alphaby2*mc_error)
  output <- list(integral=integral, mc_error=mc_error, ci = ci )
  return(output)
}

print(integral(10000,.05))


rm(list=ls(all=TRUE))

## The input parameter is #samples
integral1 <- function(n)
{
  x <- runif(n=n, min=0, max=2*pi)
  integral <- mean(2*pi*sin(x*cos(x)))
  mc_error <- sqrt(var(2*pi*sin(x*cos(x))) / n)
  
  output <- list(integral=integral, mc_error=mc_error)
  return(output)
}



#### Computation of the second integral
## The input parameter is #samples
integral2 <- function(n)
{
  x <- rnorm(n)
  integral <- mean(sin(x*cos(x)) / dnorm(x))
  mc_error <- sqrt(var(sin(x*cos(x)) / dnorm(x)) / n)
  
  output <- list(integral=integral, mc_error=mc_error)
  return(output)
}

print(integral2(10000))


X <- runif(100000,0,10)
Y <- 10*exp(-2*abs(X-5))
c( mean(Y), var(Y) )



X=rnorm(1e5,mean=5,sd=1)
fX = 10*exp(-2*abs(X-5))
Y=fX*dunif(X,0,10)/dnorm(X,mean=5,sd=1)
c( mean(Y), var(Y) )



## Density function of standard normal distribution
f <- function(x)
{
  y <- 1/sqrt(2*pi) * exp(-x^2/2)
  return(y)
}

## Density function of Cauchy distribution
g <- function(x)
{
  y <- 1 / (pi * (1 + x^2))
  return(y)
}

## Rejection sampling for standard normal distribution
rejection_sampling_standard_normal <- function(n)
{
  ## Boundary M
  M <- f(1)/g(1)
  
  ## Initialize the output
  x <- rep(0, n)
  
  k <- 0 ## Number of accepted samples
  ## While loop until there are n accepted samples
  while (k <= n)
  {
    y <- rcauchy(1)
    u <- runif(1)
    if (u*M*g(y) <= f(y))
    {
      k <- k + 1
      x[k] <- y
    }
  }
  
  return(x)
}

## Call function
x <- rejection_sampling_standard_normal(1000)
hist(x, breaks=20)

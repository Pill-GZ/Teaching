###############################################################
#Q1
###############################################################

## Simulate 100 data points from Normal distribution with mu=2, sigma^2=4
X <- rnorm(100, mean=2, sd=sqrt(4))
n <- 100

# likelihood t[1]: mu, t[2]: sigma^2
# if sigma < 0 return Inf, instead of NaN.
f <- function(t)
{print(X)
  if( t[2] > 0)
  {
    return( -sum( dnorm(X, mean=t[1], sd=sqrt(t[2]), log=TRUE) ) )
  } else
  {
    return(Inf)
  }
}

# gradient function
df <- function(t)
{
  mu <- t[1]; sig <- t[2];
  g <- rep(0,2)
  g[1] <- (1/sig) * sum(X - mu)
  g[2] <- (-n/(2*sig)) + sum( (X-mu)^2 )/(2*sig^2)
  return(-g)
}

## Run optim() 
objoptim <- optim(par=c(0, 1), fn=f, gr=df, method='BFGS')
print(objoptim$par)
###############################################################
# 2
###############################################################
# f and f'
f <- function(x) exp(x)-5
df <- function(x) exp(x)
# make the initial plot
v <- seq(0,4,length=1000)
plot(v,f(v),type="l",col=8)
abline(h=0)
# start point
x0 <- 3.5
# paste this repeatedly to see Newton's method work
segments(x0,0,x0,f(x0),col=2,lty=3)
slope <- df(x0)
g <- function(x) f(x0) + slope*(x - x0)
v = seq(x0 - f(x0)/df(x0),x0,length=1000)
lines(v,g(v),lty=3,col=4)
x0 <- x0 - f(x0)/df(x0)

###############################################################
#3
###############################################################

# f: the function you want the max of
# df: derivative of f; d2f: second derivative
# x0: start value, tol: convergence criterion
# maxit: max # of iterations before it is considered to have failed
newton <- function(x0, f, df, d2f, tol=1e-4, pr=FALSE){
  # iteration counter
  k <- 0
  # initial function, derivatives, and x values
  fval <- f(x0)
  grad <- df(x0)
  hess <- d2f(x0)
  xk_1 <- x0
  cond1 <- sqrt(sum(grad^2))
  cond2 <- Inf
  # see if the starting value is already close enough
  if( (cond1 < tol) ) return(x0)
  while( (cond1 > tol) & (cond2 > tol) )
  {
    L <- 1
    bool <- TRUE
    while(bool == TRUE){
      xk <- xk_1 - L * solve(hess) %*% grad
      # see if we've found an uphill step
      if( f(xk) > fval ){
        bool = FALSE
        grad <- df(xk)
        fval <- f(xk)
        hess <- d2f(xk)
        # make the stepsize a little smaller
      } 
      else {
        L = L/2
        if( abs(L) < 1e-20 ) return("Failed to find uphill step - try new start val")
      }
    }
    # calculate convergence criteria
    cond1 <- sqrt( sum(grad^2) )
    cond2 <- sqrt( sum( (xk-xk_1)^2 ))/(tol + sqrt(sum(xk^2)))
    # add to counter and update x
    k <- k + 1
    xk_1 <- xk
  }
  if(pr == TRUE) print( sprintf("Took %i iterations", k) )
  return(xk)
}

###############################################################
# Exercise
###############################################################

f <- function(x) exp(-(x^2))
df <- function(x) -2*x*f(x)
d2f <- function(x) -2*f(x)-2*x*df(x)
# Newton's method starting at x0 = 2/3
newton(2/3, f, df, d2f, 1e-7)


###############################################################
#4
###############################################################

X <- rnorm(100, mean=2, sd=sqrt(4))
n <- 100
# likelihood t[1]: mu, t[2]: sigma^2
# if sigma < 0 return -Inf, instead of NaN.
f <- function(t){
  if( t[2] > 0)
  { return( sum( dnorm(X, mean=t[1], sd=sqrt(t[2]), log=TRUE) ) )
  } 
  else {
    return(-Inf)
  }
}
# score function
df <- function(t){
  mu <- t[1]; sig <- t[2];
  g <- rep(0,2)
  g[1] <- (1/sig) * sum(X - mu)
  g[2] <- (-n/(2*sig)) + sum( (X-mu)^2 )/(2*sig^2)
  return(g)
}
# hessian
d2f <- function(t){
  mu <- t[1]; sig <- t[2];
  h <- matrix(0,2,2)
  h[1,1] <- -n/sig
  h[2,2] <- (n/(2*sig^2)) - sum( (X-mu)^2 )/(sig^3)
  h[1,2] <- -sum( (X-mu) )/(sig^2)
  h[2,1] <- h[1,2]
  return(h)
}
newton( c(0,1), f, df, d2f)
###############################################################
#5
###############################################################
############
#Censored regression
#Generate data
beta_star=2; n_0=100
X=rnorm(n_0,0,1)
Y=rnorm(n_0,mean=beta_star*X,sd=1)
C=2.5
n=sum(Y<=C); m=sum(Y>C)
X=c(X[Y<=C],X[Y>C])
Y=Y[Y<=C]
beta=0
K=50
Res=double(K)
for (k in 1:K){
  m_beta= X[(n+1):(n+m)]*beta + dnorm(C-X[(n+1):(n+m)]*beta)/(1-pnorm(C-X[(n+1):(n+m)]*beta))
  beta =( sum(X[1:n]*Y) + sum(X[(n+1):(n+m)]*m_beta) ) / ( sum(X^2) )
  Res[k]=beta
}
plot(Res,type='b',col='blue')






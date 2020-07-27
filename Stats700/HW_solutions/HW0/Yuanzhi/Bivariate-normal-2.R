Inv.quadform <- function(X, Sigma){
  Y = t(X) %*% solve(Sigma, X)
  return(Y)
}


library(MASS)

N = c(20,100,1000,1e4)
M = 5000
sigma.X = 1
sigma.Y = 2
rho = 0.3
#rho = 0.95
Sigma = matrix( c(sigma.X^2, sigma.X*sigma.Y*rho,
                  sigma.X*sigma.Y*rho, sigma.Y^2), 2, 2 )
mu.X = 10
mu.Y = 5

Est.rho = rep(0,length(N))
CI.rho = matrix(NA,length(N),2)

for(j in 1:length(N)){ ##Vary the sample size n
  print(j)
  n = N[j]
  rho.hat = 0
  for(i in 1:M){## Simulation starts
    #########
    ##Generating Data
    Data = mvrnorm(n, mu = c(mu.X, mu.Y), Sigma)
    
    ###########
    ## Estimating
    Est.Sigma = cov(Data)
    S.XX = Est.Sigma[1,1]*(n-1)/n
    S.YY = Est.Sigma[2,2]*(n-1)/n
    S.XY = Est.Sigma[1,2]*(n-1)/n
    
    rho.hat[i] = S.XY/sqrt(S.XX*S.YY)
  }
  Est.rho[j] = mean(rho.hat)
  CI.rho[j,] = quantile(rho.hat, c(0.025,0.975) )
}

##### Visualizing the result
## Plot the confidence intervals
opar = par()
par(cex.axis=1.5,cex.lab=1.5)
abline(h=log(rho+1,10),col='red',lty = 2)
plot(log(Est.rho+1,10)~log(N,10),xlab = 'log sample size', ylab = 'log estimate',pch=4, cex=0.8,ylim=c(-0.08,0.35))
lines(log(Est.rho+1,10)~log(N,10),xlab = 'log sample size', ylab = 'log estimate',pch=4, cex=0.8,ylim=c(-0.08,0.35))
segments(x0 = log(N,10),y0=(log(CI.rho[,1]+1,10)),y1=log(CI.rho[,2]+1,10))
segments(x0 = log(N,10)-0.1,y0 = log(CI.rho[,1]+1,10), x1=log(N,10)+0.1)
segments(x0 = log(N,10)-0.1,y0 = log(CI.rho[,2]+1,10), x1=log(N,10)+0.1)

lines(log(Est.rho1+1,10)~log(N,10),xlab = 'log sample size', ylab = 'log estimate',pch=4, cex=0.8,ylim=c(-0.08,0.23), col='purple')
points(log(Est.rho1+1,10)~log(N,10),xlab = 'log sample size', ylab = 'log estimate',pch=4, cex=0.8,ylim=c(-0.08,0.23), col='purple')
segments(x0 = log(N,10),y0=(log(CI.rho1[,1]+1,10)),y1=log(CI.rho1[,2]+1,10), col='purple')
segments(x0 = log(N,10)-0.1,y0 = log(CI.rho1[,1]+1,10), x1=log(N,10)+0.1, col='purple')
segments(x0 = log(N,10)-0.1,y0 = log(CI.rho1[,2]+1,10), x1=log(N,10)+0.1, col='purple')
legend('bottom',legend = c(expression(rho==0.3),expression(rho==0.95)),
       lwd=2,pt.cex=1.2,cex=, lty = c(2,2), pch=c(4,4),col=c('black','purple'),y.intersp =0.35)

## Plot the relative bias
plot((Est.rho-rho)/rho~log(N,10),xlab = 'log sample size', ylab = 'Relative bias', cex = 0.8, type = 'b')
lines((Est.rho1-rho1)/rho1~log(N,10),xlab = 'log sample size', ylab = 'Relative bias', cex = 0.8, type = 'b', col='purple')
abline(h=0,col='red', lty = 2)
legend('bottomright',legend = c(expression(rho==0.95), expression(rho==0.3)),lwd=2,pt.cex=1.2,cex=1, lty = c(1,1), pch=c(1,1),col=c('purple','black'),y.intersp =0.35)

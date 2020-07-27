Inv.quadform <- function(X, Sigma){
 Y = t(X) %*% solve(Sigma, X)
 return(Y)
}


library(MASS)

N = c(20,100,500,2000)
M = 5000
sigma.X = 1
sigma.Y = 1
rho = 0.3
Sigma = matrix( c(sigma.X^2, sigma.X*sigma.Y*rho,
                  sigma.X*sigma.Y*rho, sigma.Y^2), 2, 2 )
mu.X = 10
mu.Y = 10
Fin.Width.Fieller = rep(NA,length(N))
Fin.Width.Delta = Fin.Width.Fieller
Fin.Cover.Delta = Fin.Width.Fieller
Fin.Cover.Fieller = Fin.Width.Fieller

for(j in 1:length(N)){ ##Vary the sample size n
  print(j)
  Cover.Fieller = 0
  Cover.Delta = 0
  Width.Fieller = 0
  Width.Delta = 0
n = N[j]
for(i in 1:M){## Simulation starts
#########
##Generating Data
Data = mvrnorm(n, mu = c(mu.X, mu.Y), Sigma)

###########
## Estimating Delta method
mu.hat = apply(Data, 2, mean)
Data.centered = apply(Data, 1, '-', mu.hat )
sigma2.hat = sum( apply(Data.centered, 2, Inv.quadform, Sigma) )/(2*n)
Sigma.hat = matrix( c(sigma2.hat/n, rho*sigma2.hat/n, rho*sigma2.hat/n,
                      sigma2.hat/n), 2, 2 )
temp.vec = c(-mu.hat[2]/mu.hat[1]^2, 1/mu.hat[1])
Delta.var.est = t(temp.vec) %*% Sigma.hat %*% temp.vec
Delta.CI = mu.hat[2]/mu.hat[1] + c( -qnorm(0.975) * sqrt(Delta.var.est),
                                    qnorm(0.975) * sqrt(Delta.var.est) )

## Estimating Fieller's method
a = mu.hat[2]
b = mu.hat[1]
t.quant = qt(0.975, n-2)
f1 = a*b - t.quant^2 * rho * sigma2.hat / n
f2 = b^2 - t.quant^2 * sigma2.hat / n
f0 = a^2 - t.quant^2 * sigma2.hat / n
D = f1^2 - f0*f2
Fieller.CI = c((f1 - sqrt(D)) / f2, (f1 + sqrt(D)) / f2)

## Evaluate the width and coverage rate
Width.Fieller = Width.Fieller +  diff(Fieller.CI)
Width.Delta = Width.Delta +  diff(Delta.CI)

if(Fieller.CI[1]<=1 && Fieller.CI[2]>=1) Cover.Fieller = Cover.Fieller + 1
if(Delta.CI[1]<=1 && Fieller.CI[2]>=1) Cover.Delta = Cover.Delta + 1
}

Fin.Width.Fieller[j] = Width.Fieller / M
Fin.Width.Delta[j] = Width.Delta / M

Fin.Cover.Fieller[j] = Cover.Fieller / M
Fin.Cover.Delta[j] = Cover.Delta / M
 }
# opar= par()
# par(cex.axis=1.5,cex.lab=1.5)
plot(Fin.Cover.Delta~log(N), type= 'b', ylim=c(0.94,0.96), pch = 4, xlab = 'log sample size', ylab = 'Coverage rates')
points(Fin.Cover.Fieller~log(N), type='b', pch = 5,col='blue')
abline(h=0.95, col = 'red',lty=2)
legend('topright',legend = c('Delta','Fieller','Nominal level'),lwd=2,pt.cex=1.2,cex=0.8, lty = c(1,1,2), pch=c(4,5,-1),col=c('black','blue','red'),y.intersp =0.35)
# 
# # plot((Fin.Width.Delta)~log(N), type= 'b', ylim=(c(0.01,0.12)), pch = 4, xlab = 'log sample size', ylab = 'Average width')
# # points((Fin.Width.Fieller)~log(N), type='b', pch = 5, col='blue')
# # legend('topright',legend = c('Delta','Fieller'),lwd=2,pt.cex=1.2,cex=0.9, lty = c(1,1), pch=c(4,5),col=c('black','blue'),y.intersp =0.35)
# 

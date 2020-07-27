set.seed(133)
m = c(1e2,1e3,1e4,1e5)
theta = c(1,1e1,1e2,1e3)
Color.name = c('black','blue','purple','green3')

par(cex.axis=1.5,cex.lab=1.5)

for(i in 1:4){ ## Varying m
  pdf(paste('GP',i,'.pdf'),8,6)
  plot(0,0,col='white', main = NULL, ylab = 'Density', xlab = expression( (Y-EY)/sqrt(var(Y))  ),
       xlim = c(-5,5), ylim = c(0,0.7) )
  legend('topright',legend = c('Standard normal', expression(theta==1),expression(theta==10),expression(theta==100),expression(theta==1000)),
         lwd=2,pt.cex=1.2,cex=0.8, lty = c(2,1,1,1,1),col=c('red',Color.name),y.intersp =1, bty = 'n')
  for (j in 1:4){ ## Varying theta
    rnd.N = rpois(m[i], theta[j])
    rnd.Y = rchisq(m[i], 2*rnd.N)
    Z = (rnd.Y - 2*theta[j])/sqrt(8*theta[j])
    lines( density( Z), col = Color.name[j] )
    lines( seq(-5,5,0.1),dnorm(seq(-5,5,0.1)) , col='red', lty = 2)
  }
  
  dev.off()
}

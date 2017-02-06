###########################################
### 1. Random number generation
###########################################


### Inversion method: exponential distribution

set.seed(2015);
n = 10000;
lambda = 3;
x = -(1/lambda)*log(runif(n));
hist(x, prob = TRUE, main="Inversion method: exponential r.v.s");
## (Recap last lab session: what are the following two lines doing?)
x_dist = seq(0, 10, length.out = 1000);
lines(x=x_dist, y=dexp(x_dist, lambda), col='blue');



### Inversion method: Pareto distribution

# requires package: actuar
if("actuar" %in% rownames(installed.packages()) == FALSE){
  install.packages("actuar", dep=TRUE);
}
require(actuar);

# Preview the distribtion
## (Study the code below as a good review of last lab session on plotting)
x_preview = seq(from=0.05, to=3, by=0.05);
matplot(x=x_preview,
        y=cbind(dpareto(x_preview, shape=1, scale=1), 
                dpareto(x_preview, shape=3, scale=1), 
                dpareto(x_preview, shape=5, scale=1)),
        col=1:3, lty=1, type='l',
        xlab='x', ylab='density', main='PDF for Pareto distribution, scale=1',
        cex.lab=1.5, cex.axis=1.5, cex.main=1.5);
legend('topright', legend=c('shape=1', 'shape=3', 'shape=5'), col=1:3, lty=1, cex=1.5);

# Sample from a Pareto distribution
set.seed(2015);
n = 1000000;
x_m=1; alpha=3;
# x = rpareto(n, shape=alpha, scale=x_m); # backup for verification
x = x_m*(1-runif(n))^(-1/alpha) - 1; # minus one to set location so that domain starts at 0
## (Recap the usage of paste() from last lab session)
hist(x, probability=TRUE, breaks=500, xlim=c(0, 5), 
     main=paste('Inversion method: Pareto r.v.s, shape=', alpha, ', scale=', x_m, sep=''));
x_dist = seq(0.01, 3, length.out = 1000);
lines(x=x_dist, y=dpareto(x_dist, shape=alpha, scale=x_m), col='blue');




### Rejection method

# Set up the funciton
bimodal = function(x) -(x-1)*(x-3)*((abs(x-2)+0.001)^(2/3)+1)^2;
normalizing_constant = integrate( bimodal, 1, 3 )$value;
bimodal = function(x) -(x-1)*(x-3)*((abs(x-2)+0.001)^(2/3)+1)^2/normalizing_constant;

# Sampling
set.seed(2015);
n = 30000;
count_accepted = 0;
sample_bimodal = c();
while(count_accepted<n){
    x = rnorm(1, mean=2, sd=1);
    if( x<1 | x>3 ){
      next;
    } else if( runif(1)<bimodal(x)/2*dnorm(x, mean=2, sd=1) ){
        sample_bimodal = c(sample_bimodal, x);
        count_accepted = count_accepted + 1;
    }
}
hist(sample_bimodal, prob = TRUE, breaks=50, xlim=c(0, 4), ylim=c(0, 1),
     xlab="x", ylab='Density', main="Rejection sampling example",
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5);
x_dist = seq(0.01, 4, length.out = 1000);
lines(x=x_dist, y=2*dnorm(x_dist, mean=2, sd=1), col='blue');
legend("topleft", legend="2*PDF(N(2, 1))", lty=1, col="blue");




### Linear transformations(for multivariate normal distributions)

# Generate sample from standard multivariate normal distribution
set.seed(2015);
require(MASS);
n = 1000; p = 2; # p = 2: we set this example to sit in two-dimensional space to facilitate visualization
mu = c(1,1); Sigma = matrix(c(1,0.1,0.1,1), c(2,2));
X_standard = matrix(rnorm(n*p), c(n,p));
# Transformation
# Figure out the square-root matrix
svd.Sigma = svd(Sigma);
Sigma.sqrt = svd.Sigma$u %*% diag(sqrt(svd.Sigma$d)) %*% t(svd.Sigma$u);
X = X_standard %*% Sigma.sqrt + rep(1, n) %*% t(mu);
# Visualization
result = kde2d(x=X[,1], y=X[,2]);
par(new=FALSE);
image(result, col=rev(heat.colors(20)), xlim=c(-2, 4), ylim=c(-2, 4), axes=FALSE); par(new=TRUE);
contour(result, xlim=c(-2, 4), ylim=c(-2, 4), col='blue');
par(new=FALSE);



###########################################
### 2. Empirical CDF
###########################################


### Standrd normal distribution example

n_vec = round( exp(seq(1.6, 8, by=0.4)) );

# Normal distribution

# Animated illustration of concentration
set.seed(2015);
# x11();
for(n in n_vec){
    # draw sample  
    sample_normal = rnorm(n);
    # compute empirical CDF
    Fhat = ecdf(sample_normal); # Use R's build-in empirical CDF function
    # plot
    plot(Fhat, verticals=TRUE, xlim=c(-3, 3), ylim=c(0, 1), col.hor="black", col.vert="bisque", main=paste("Empirical CDF for N(0, 1)\nSample size = ", n, sep=""));
    # draw reference line
    x_dist = seq(-3, 3, by=0.01);
    y_dist = pnorm(x_dist);
    lines(x=x_dist, y=y_dist, col='blue');
    # Pause for each frame
    Sys.sleep(2);
}
# Sys.sleep(10);
# dev.off();


# Cauchy distribution
set.seed(2015);
for(n in n_vec){
    # draw sample  
    sample_cauchy = rcauchy(n);
    # compute empirical CDF
    Fhat = ecdf(sample_cauchy); # Use R's build-in empirical CDF function
    # plot
    plot(Fhat, verticals=TRUE, xlim=c(-3, 3), ylim=c(0, 1), col.hor="black", col.vert="bisque", main=paste("Empirical CDF for Cauchy\nSample size = ", n, sep=""));
    # draw reference line
    x_dist = seq(-3, 3, by=0.01);
    y_dist = pcauchy(x_dist);
    lines(x=x_dist, y=y_dist, col='blue');
    # Pause for each frame
    Sys.sleep(2);
}

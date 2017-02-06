### Lab 5


### 1. Illustration of rejection method

rm(list=ls());

# target distribution up to a normalizing constant
# truncated normal distribution
f = function(x, threshold) (x>threshold)*dnorm(x); # NOTICE: f(x) as is is NOT a density function since it does not integrate to 1.

# envoloping distribution
g = function(x, location) dexp(x-location);

# set threshold
threshold = 3;
M = f(threshold+1e-10, threshold) / g(threshold+1e-10, threshold);

## illustrate the rejection sampling procedure.
set.seed(2015);
n = 5000; # number of points sampled

# start iteration
sample.accepted = c();
sample.accepted.yaxis = c();
sample.rejected = c();
sample.rejected.yaxis = c();

while(length(sample.accepted)<n){
    # sample from the envoloping distribution
    candidate = rexp(1) + threshold;
    # given the realization of the r.v. drawn from the envoloping distribution, determine acceptance or rejection
    candidate.yaxis = runif(1, min=0, max=M*g(candidate, threshold));
    if(candidate.yaxis<f(candidate, threshold)){
        sample.accepted = c(sample.accepted, candidate);
        sample.accepted.yaxis = c(sample.accepted.yaxis, candidate.yaxis);
    } else {
        sample.rejected = c(sample.rejected, candidate);
        sample.rejected.yaxis = c(sample.rejected.yaxis, candidate.yaxis);
    }
}

# plot
x.plot.vec = seq(from=threshold+1e-7, to=threshold+4, length.out=1000);
matplot(x=x.plot.vec, y=cbind(f(x.plot.vec, threshold), M*g(x.plot.vec, threshold)),
        col = c('black', 'blue'), lty=1, type='l',
        main=paste('Standard normal truncated at threshold=', threshold, ' with n=', n, sep=''), xlab='X', ylab='pdf',
        cex.main=1.5, cex.lab=1.5, cex.axis=1.5);
legend('topright', legend=c('f(x)', 'M*g(x)', 'accepted', 'rejected'), 
       col=c('black', 'blue', 'darkgreen', 'darkred'), lty=c(1,1,NA,NA), pch=c(NA,NA,1,4), cex=1.5);
points(x=sample.accepted, y=sample.accepted.yaxis, col='darkgreen', pch=1);
points(x=sample.rejected, y=sample.rejected.yaxis, col='darkred', pch=4);

# Compare the empirical distribution with the theoretical distribution
# NOTICE: require larger n for a good approximation!!
contain.in.interval = sample.accepted>threshold+0.1 & sample.accepted<threshold+0.6;
proportion.empirical = mean(contain.in.interval);
proportion.empirical.se = sd(contain.in.interval) / sqrt(length(contain.in.interval));
proportion.theoretical = ( pnorm(threshold+0.6) - pnorm(threshold+0.1) ) / (1-pnorm(threshold));
cat( paste('estimated = ', round(proportion.empirical, 3), '(+/-)', round(2*proportion.empirical.se, 3), ' ---  95% CI\n',
          'theoretical = ', round(proportion.theoretical, 3), '\n', sep='') );







### 2. Monte Carlo integration

rm(list=ls());

# defining the integrand
# OK to throw away parts near 0, will have ignorable effect the integral value.
f = function(x){
    if(abs(x)>1e-10){
        return(sin(cos(x)/x^3));
    } else{
        return(0);
    }
}

# start Monte Carlo integration
set.seed(2015);
n = 1e7; ### INCREASE n by adding 0's!!!

## Approach 1(good): using pi(x) = PDF(Cauchy; x)
sample.cauchy = rcauchy(n);
sample.integrand.cauchy = unlist(lapply(sample.cauchy, f)) / unlist(lapply(sample.cauchy, dcauchy));
I.cauchy = mean(sample.integrand.cauchy);
var.I.cauchy = var(sample.integrand.cauchy);#/sqrt(n);

cat(paste('mean=', round(I.cauchy, 3), ',  var=', round(var.I.cauchy, 3), '\n', sep=''));


## Approach 2(bad): using pi(x) = PDF(Normal; x)
sample.norm = rnorm(n);
sample.integrand.norm = unlist(lapply(sample.norm, f)) / unlist(lapply(sample.norm, dnorm));
I.norm = mean(sample.integrand.norm);
var.I.norm = var(sample.integrand.norm);#/sqrt(n);

cat(paste('mean=', round(I.norm, 3), ',  var=', round(var.I.norm, 3), '\n', sep=''));

## NOTICE: normally here we should report standard error instead of standard deviation,
##         but we just wanted to emphasize the point that using a normal sample explodes the sd to infinity.

# results:
# n = 1e7:
# (cauchy): mean=0.001,  var=5.173
# (normal): mean=-0.001,  var=64.696



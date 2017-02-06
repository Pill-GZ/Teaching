### Example 2 of Lab 6 revisited

### High-dimensional integration

# Define integrand
sd_x=sqrt(10);
g = function(x_vec) (sum(x_vec^2)<1) * 10 *exp(-sum(x_vec^2)/(2*(sd_x)^2));

# Parameters
n = 50;

# Method 1: Naive Monte Carlo
set.seed(2015);
sample.naiveMC = c();
sample.naiveMC.weight = c();
for(i in 1:n){
  X = runif(4, min=-1, max=1);
  while(sum(X^2)>1){
	X = runif(4, min=-1, max=1);
  }
  sample.naiveMC = c(sample.naiveMC, g(X));
  sample.naiveMC.weight = c(sample.naiveMC.weight, 1);
}

# Method 2: Importance sampling
VolumeUnitBallR4 = pi^2/2;

set.seed(2015);
sample.IS.primary = c();
sample.IS.primary.weight = c();

for(i in 1:n){
  X = rnorm(4, sd=sd_x);
  while(sum(X^2)>1){
    X = rnorm(4, sd=sd_x);
  }
  sample.IS.primary = c(sample.IS.primary, g(X));
  # NOTICE:
  # 1. It is a truncated multivariate normal distribution that we are sampling from
  # 2. Therefore, the pdf for this distribution, on its support, 
  #    is the density of its ordinary(untruncated) version divided by the probability
  #    that |X|_2^2<1.
  #    Finally, notice |X|_2^2 follows a Chi-squared distribution (degrees of freedom = 4).
  sample.IS.primary.weight = c(sample.IS.primary.weight, (1/VolumeUnitBallR4) * pchisq(1/sd_x^2, 4)/prod(dnorm(X, sd=sd_x)));
}


# Method 3: Importance sampling (self-normalized form)
set.seed(2015);
sample.IS.variant = c();
sample.IS.variant.weight = c();

for(i in 1:n){
  X = rnorm(4, sd=sd_x);
  while(sum(X^2)>1){
    X = rnorm(4, sd=sd_x);
  }
  sample.IS.variant = c(sample.IS.variant, g(X));
  # NOTICE:
  # 1. It is a truncated multivariate normal distribution that we are sampling from
  # 2. No need to figure out the normalizing constant here.
  sample.IS.variant.weight = c(sample.IS.variant.weight, 1/prod(dnorm(X, sd=sd_x))); # NOTICE the different in this line.
}

# NOTICE!!
#   Here, we only computed the mean of g(x) in the unit ball.
#   In order to obtain the integral, we need to multiply it by the volume of the 4-dimensional ball!!
# end NOTICE!!


## Rule of thumb
n_e = (sum(sample.IS.variant.weight))^2 / sum(sample.IS.variant.weight^2);

### Output results

cat(paste('---------------------------------------------------------\n',
	    'Naive Monte Carlo:\n',
          'Sample mean = ', round(mean(sample.naiveMC*sample.naiveMC.weight), 3), 
          '; sample variance = ', round(var(sample.naiveMC*sample.naiveMC.weight), 3), '\n',
          '---------------------------------------------------------\n',
          'Importance sampling (primary form):\n',
          'Sample mean = ', round(mean(sample.IS.primary*sample.IS.primary.weight), 3),
          '; sample variance = ', var(sample.IS.primary*sample.IS.primary.weight), '\n',
          '---------------------------------------------------------\n',
          'Importance sampling (self-normalized form):\n',
          'Sample mean = ', round(sum(sample.IS.variant*sample.IS.variant.weight)/sum(sample.IS.variant.weight), 3),
          '; sample variance = ', n*var(sample.IS.variant*sample.IS.variant.weight)/(sum(sample.IS.variant.weight))^2, '\n',
          sep=''));
cat('---------------------------------------------------------\n');
cat(paste('Rule of thumb: ESS-IS=', round(n_e, 3), '\n', sep=''));
cat('---------------------------------------------------------\n');



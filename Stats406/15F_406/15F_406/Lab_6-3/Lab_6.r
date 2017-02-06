### Relative efficiency between naive Monte Carlo integration and integration using importance sampling

### Example 1

# Define integrand
f = function(x, y) 0.5*exp(-90*(x-0.5)^2-45*(y+0.1)^4) + exp(-45*(x+0.4)^2-60*(y-0.5)^2);

# Parameters
n = 5000;

# Method 1: Naive Monte Carlo
set.seed(2015);
sample.naiveMC = c();
sample.naiveMC.weight = c();
# record the locations of sampled (X, Y), not necessary but just for illustration
sample.naiveMC.X = c();
sample.naiveMC.Y = c();
for(i in 1:n){
      X = runif(1, min=-1, max=1);
      Y = runif(1, min=-1, max=1);
      sample.naiveMC = c(sample.naiveMC, f(X, Y));
      sample.naiveMC.weight = c(sample.naiveMC.weight, 4); # The density here is always 1/4
      sample.naiveMC.X = c(sample.naiveMC.X, X);
      sample.naiveMC.Y = c(sample.naiveMC.Y, Y);
}

# Method 2: Importance sampling
set.seed(2015);
sample.IS = c();
sample.IS.weight = c();
sample.IS.X = c();
sample.IS.Y = c();

C1 = 0.5*sqrt((1/180)*(1/20));
C2 = 1*sqrt((1/90)*(1/120));
mixture.probability = C1/(C1+C2);

for(i in 1:n){
      X = 100; Y = 100;
      while(abs(X)>1 | abs(Y)>1){
            which.normal = rbinom(1, 1, mixture.probability);
            if(which.normal==1){
                  X = rnorm(1, mean=0.5, sd=sqrt(1/180));
                  Y = rnorm(1, mean=-0.1, sd=sqrt(1/20));
            } else {
                  X = rnorm(1, mean=-0.4, sd=sqrt(1/90));
                  Y = rnorm(1, mean=0.5, sd=sqrt(1/120));
            }
      }
      sample.IS = c(sample.IS, f(X, Y));
      sample.IS.weight = c(sample.IS.weight, 
                           1/(
                           mixture.probability *
                           dnorm(X, mean=0.5, sd=sqrt(1/180)) *
                           dnorm(Y, mean=-0.1, sd=sqrt(1/20)) +
                           (1-mixture.probability) *
                           dnorm(X, mean=-0.4, sd=sqrt(1/90)) *
                           dnorm(Y, mean=0.5, sd=sqrt(1/120))
                           )
                           );
      sample.IS.X = c(sample.IS.X, X);
      sample.IS.Y = c(sample.IS.Y, Y);
}

# Output
cat(paste('Naive Monte Carlo:\n',
          'Sample mean = ', round(mean(sample.naiveMC*sample.naiveMC.weight), 3), 
          '; sample variance = ', round(var(sample.naiveMC*sample.naiveMC.weight), 3), '\n',
          'Importance sampling:\n',
          'Sample mean = ', round(mean(sample.IS*sample.IS.weight), 3),
          '; sample variance = ', round(var(sample.IS*sample.IS.weight), 3), '\n',
          sep=''));







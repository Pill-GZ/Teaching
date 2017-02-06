### Relative efficiency between naive Monte Carlo integration and integration using importance sampling

### Example 1

# Define integrand
f = function(x, y) 0.5*exp(-90*(x-0.5)^2-45*(y+0.1)^4) + exp(-45*(x+0.4)^2-60*(y-0.5)^2);

# Parameters
n = 2000;

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


# Illustration of sampling efficiency
m=500;
FuncValueMesh = array(0, c(m, m));
ValueVec = seq(from=-1, to=1, length.out=m);
for (i in 1:m){
      for (j in 1:m){
            FuncValueMesh[i, j] = f(ValueVec[i], ValueVec[j]);
      }
}


image(ValueVec, ValueVec, FuncValueMesh,
      col = heat.colors(20)[20:1],
      xlab='x', ylab='y', xlim=c(-1,1), ylim=c(-1,1),
      main = 'Naive Monte Carlo sampling',
      cex.axis=1.5, cex.lab=1.5, cex.main=1.5);
box(lty=1, col='grey');
points(sample.naiveMC.X, sample.naiveMC.Y, col='green', pch='.', cex=3.5);


image(ValueVec, ValueVec, FuncValueMesh,
      col = heat.colors(20)[20:1],
      xlab='x', ylab='y', xlim=c(-1,1), ylim=c(-1,1),
      main = 'Importance sampling',
      cex.axis=1.5, cex.lab=1.5, cex.main=1.5);
box(lty=1, col='grey');
points(sample.IS.X, sample.IS.Y, col='blue', pch='.', cex=3.5);












### Example 2
### High-dimensional integration

# Define integrand
g = function(x_vec) (sum(x_vec^2)<1) * 10*exp(-sum(x_vec^2)/2);

# Parameters
n = 5000;

# Method 1: Naive Monte Carlo
set.seed(2015);
sample.naiveMC = c();
sample.naiveMC.weight = c();
for(i in 1:n){
      X = runif(4);
      sample.naiveMC = c(sample.naiveMC, g(X));
      sample.naiveMC.weight = c(sample.naiveMC.weight, 16); # The density here is always 1/4
}

# Method 2: Importance sampling
set.seed(2015);
sample.IS = c();
sample.IS.weight = c();

for(i in 1:n){
      X = rep(100, 4);
      while(sum(X^2)>1){
            X = rnorm(4);
      }
      sample.IS = c(sample.IS, g(X));
      # NOTICE:
      # 1. It is a truncated multivariate normal distribution that we are sampling from
      # 2. Therefore, the pdf for this distribution, on its support, 
      #    is the density of its ordinary(untruncated) version divided by the probability
      #    that |X|_2^2<1.
      #    Finally, notice |X|_2^2 follows a Chi-squared distribution (degrees of freedom = 4).
      sample.IS.weight = c(sample.IS.weight, pchisq(1, 4)/prod(dnorm(X)));
}

cat(paste('Naive Monte Carlo:\n',
          'Sample mean = ', round(mean(sample.naiveMC*sample.naiveMC.weight), 3), 
          '; sample variance = ', round(var(sample.naiveMC*sample.naiveMC.weight), 3), '\n',
          'Importance sampling:\n',
          'Sample mean = ', round(mean(sample.IS*sample.IS.weight), 3),
          '; sample variance = ', round(var(sample.IS*sample.IS.weight), 3), '\n',
          sep=''));

# As a reference, check Lab_6.nb for an evaluation of the integral by deterministic numerical methods.








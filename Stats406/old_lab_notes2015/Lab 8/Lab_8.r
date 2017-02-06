
### Example 1: bootstrap MSE under N(\mu, \sigma^2)

mu = 1;
sigma = 1;
n = 25;
K = 200; # Number of bootstrap samples

set.seed(2015);
X = rnorm(n, mean=mu, sd=sigma);
mu_hat = mean(X);

# Bootstrap
BP_Samples = array(0, c(K, n));
for(k in 1:K){
      BP_Samples[k, ] = sample(X, n, replace=TRUE);
}
# Bootstrap statistics
Mu_hat_star = apply(BP_Samples, 1, mean);

# Plot
hist(Mu_hat_star, xlim=c(0, 1.5), ylim=c(0, 3), prob=TRUE);
      x_dist = seq(0, 1.5, by=0.01);
      y_dist = dnorm(x_dist, mean=mu, sd=sigma/sqrt(n));
      lines(x=x_dist, y=y_dist, col='blue');
# Bootstrap confidence interval by bootstrap percentile method
alpha = 0.05;
BP_CI_lower = quantile(Mu_hat_star, alpha/2);
BP_CI_upper = quantile(Mu_hat_star, 1-alpha/2);
cat( paste(round(100*(1-alpha)), '% CI = [', round(BP_CI_lower, 3), ', ', round(BP_CI_upper, 3), ']\n', sep='') );


### Example 2: bootstrap confidence interval by bootstrap percentile method

lambda = 1;
n = 25;
K = 200; # Number of bootstrap samples

set.seed(2015);
X = rexp(n, rate=lambda);
## For percentile method, no need to estimate \hat{\lambda} from X because we do not use it.

# Bootstrap
BP_Samples = array(0, c(K, n));
for(k in 1:K){
      BP_Samples[k, ] = sample(X, n, replace=TRUE);
}
# Bootstrap statistics
Lambda_hat_star = apply(BP_Samples, 1, function(x) 1/mean(x));

# Plot to see how well the bootsrtap distribution of lambda approximates the true distribution of lambda_hat
## 1. bootstrap distribution
hist(Lambda_hat_star, prob=TRUE);
## 2. true distribution
## In order to obtain this, we need to draw many independent samples from the population distribution
Lambda_iid_samples = c();
for(i in 1:2000){
      sample_temp = rexp(n, rate=lambda);
      Lambda_iid_samples = c(Lambda_iid_samples, 1/mean(sample_temp));
}
#par(new=TRUE);
PDF_lambda_hat = density(Lambda_iid_samples);
lines(PDF_lambda_hat, col='blue');

# Bootstrap confidence interval by bootstrap percentile method
alpha = 0.05;
BP_CI_lower = quantile(Lambda_hat_star, alpha/2);
BP_CI_upper = quantile(Lambda_hat_star, 1-alpha/2);
cat( paste(round(100*(1-alpha)), '% CI = [', round(BP_CI_lower, 3), ', ', round(BP_CI_upper, 3), ']\n', sep='') );
## Check its coverage probability:
## We need to carry out the bootstrap procedure many times
coverage_count = 0;
n_experiments = 2000;
for(experiment_ind in 1:n_experiments){
      # Redo the bootstrap (including drawing the orignal sample X) many times.
      X = rexp(n, rate=lambda);
      BP_Samples = array(0, c(K, n));
      for(k in 1:K){
            BP_Samples[k, ] = sample(X, n, replace=TRUE);
      }
      Lambda_hat_star = apply(BP_Samples, 1, function(x) 1/mean(x));
      if( lambda>quantile(Lambda_hat_star, alpha/2) & lambda<quantile(Lambda_hat_star, 1-alpha/2) ){
            coverage_count = coverage_count + 1;
      }
}
cat( paste('Coverage probability = ', round(coverage_count/n_experiments, 3), '\n', sep='') );





### Example 3: Bootstrap vs CLT
## In this example we compare the accuracies of confidence intervals computed using bootstrap and CLT

df = 1;
n = 30; # should be set greater than 30
K = 200;

alpha = 0.05; # 1 - (confidence level)

set.seed(2015);
X = rchisq(n, df=df);
mu_hat = mean(X);
se_hat = sd(X)/sqrt(n);

# Method 1: bootstrap
BP_Samples = array(0, c(K, n));
for(k in 1:K){
      BP_Samples[k, ] = sample(X, n, replace=TRUE);
}
Mu_hat_star = apply(BP_Samples, 1, mean);
BP_CI_lower = quantile(Mu_hat_star, alpha/2);
BP_CI_upper = quantile(Mu_hat_star, 1-alpha/2);

# Method 2: CLT
CLT_CI_lower = mu_hat + qnorm(alpha/2)*se_hat;
CLT_CI_upper = mu_hat + qnorm(1-alpha/2)*se_hat;


# True CI
n_experiments = 2000;
Mu_iid_Samples = c();
for(experiment_ind in 1:n_experiments){
      X = rchisq(n, df=df);
      Mu_iid_Samples = c(Mu_iid_Samples, mean(X));
}
True_CI_lower = quantile(Mu_iid_Samples, alpha/2);
True_CI_upper = quantile(Mu_iid_Samples, 1-alpha/2);

# Output results
cat( paste('Bootstrap CI = [', round(BP_CI_lower, 3), ', ', round(BP_CI_upper, 3), ']\n', sep='') );
cat( paste('CLT CI = [', round(CLT_CI_lower, 3), ', ', round(CLT_CI_upper, 3), ']\n', sep='') );
cat( paste('True CI = [', round(True_CI_lower, 3), ', ', round(True_CI_upper, 3), ']\n', sep='') );

hist(Mu_iid_Samples, prob=TRUE);


### Exercise: bootstrap bias estimation

n = 25;
lambda = 1;
K = 200;

set.seed(2015);
X = rpois(n, lambda=lambda);
p_hat = exp(-2*mean(X));

# Bootstrap
BP_Samples = array(0, c(K, n));
for(k in 1:K){
      BP_Samples[k, ] = sample(X, n, replace=TRUE);
}
# Bootstrap statistics
P_hat_star = apply(BP_Samples, 1, function(x) exp(-2*mean(x)));

# Bootstrap bias estimation
bias_BP = mean(P_hat_star-p_hat);

# Compare to true value
## Compute the true value
n_experiments = 2000;
P_iid_samples = c();
for(experiment_ind in 1:n_experiments){
      X = rpois(n, lambda=lambda);
      p_hat = exp(-2*mean(X));
      P_iid_samples = c(P_iid_samples, p_hat);
}
bias_true = mean(P_iid_samples-exp(-2*lambda));

cat( paste('Bootstrap bias estimation = ', round(bias_BP, 3), '\n', 'True bias = ', round(bias_true, 3), '\n', sep='') );

# Plot
hist(P_iid_samples-exp(-2*lambda), prob=TRUE, main='Histogram of individual biases');
abline(v=bias_true, col='red');
abline(v=bias_BP, col='blue', lty=2);






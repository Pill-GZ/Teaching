
##### EM algorithm for Gaussian mixture

set.seed(2015);

### Parameter set up
mu_1_true    = -2;
mu_2_true    = 2;
sigma_1_true = 1;
sigma_2_true = 0.5;
p_mix_true   = 0.7; ## 70% from F_1, 30% from F_2

### Data generation
n = 2000;
Z_true    = rbinom(n, 1, p_mix_true);
X_F1 = rnorm(n, mean=mu_1_true, sd=sigma_1_true);
X_F2 = rnorm(n, mean=mu_2_true, sd=sigma_2_true);
X = Z_true*X_F1 + (1-Z_true)*X_F2;

# (Visualize the data)
hist(X, nclass=50);

### The EM algorithm
N_iteration_EM = 2000;

# Initial guesses for parameters
Mu_1    = c(-1,  rep(0, N_iteration_EM));
Mu_2    = c(1,   rep(0, N_iteration_EM));
Sigma_1 = c(1,   rep(0, N_iteration_EM));
Sigma_2 = c(1,   rep(0, N_iteration_EM));
P_mix   = c(0.5, rep(0, N_iteration_EM));

for (k in 2:(N_iteration_EM+1)){
	# E-step: evaluate the conditional expectation of Z_i's
	Term_1 = exp(-(X-Mu_1[k-1])^2/(2*Sigma_1[k-1]^2))*P_mix[k-1]/Sigma_1[k-1];
	Term_2 = exp(-(X-Mu_2[k-1])^2/(2*Sigma_2[k-1]^2))*(1-P_mix[k-1])/Sigma_2[k-1];
	Z = Term_1 / (Term_1+Term_2);
	
	# M-step: update estimated parameters
	Mu_1[k]    = sum(Z*X)/sum(Z);
	Mu_2[k]    = sum((1-Z)*X)/sum(1-Z);
	P_mix[k]   = mean(Z);
	Sigma_1[k] = sqrt(sum(Z*(X-Mu_1[k])^2)/sum(Z));
	Sigma_2[k] = sqrt(sum((1-Z)*(X-Mu_2[k])^2)/sum(1-Z));
}
print(round(c(Mu_1[N_iteration_EM+1], Mu_2[N_iteration_EM+1], Sigma_1[N_iteration_EM+1], Sigma_2[N_iteration_EM+1], P_mix[N_iteration_EM+1]), 3));



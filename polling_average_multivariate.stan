data {
  int<lower=0> T; // # of Time steps
  int<lower=0> K; // # of Pollsters
  int<lower=0> J; // # of Parties
  int<lower=0> N; // # of Observations
  int<lower=1, upper=K> k[N]; // Pollster
  int<lower=1, upper=J> j[N]; // Party
  int<lower=1, upper=T> t[N]; // Time step
  vector[N] vi; // Vote intention observations 
  vector[J] vote0; // Initial vote states; take from election results
}

parameters {
  matrix[J,T] vote; // True states/polling average
  vector<lower=0>[J] sigma_obs; // Observation noise
  matrix[J,K] mu; // Pollster effect
  cholesky_factor_corr[J] L_Omega_mu; // Cholesky factor of correlation matrix for mu
  cholesky_factor_corr[J] L_Omega_vote; // Cholesky factor of correlation matrix for vote
  vector<lower=0>[J] tau_mu; // Marginal standard deviations for mu
  vector<lower=0>[J] tau_vote; // Marginal standard deviations for vote
}

model {
  // Priors
  sigma_obs ~ cauchy(0, 2.5);
  L_Omega_mu ~ lkj_corr_cholesky(2);
  L_Omega_vote ~ lkj_corr_cholesky(2);
  tau_mu ~ cauchy(0, 2.5);
  tau_vote ~ cauchy(0, 2.5);

  // Multivariate normal prior for pollster effect for each party
  for (l in 1:K) {
    mu[:, l] ~ multi_normal_cholesky(rep_vector(0, J), diag_pre_multiply(tau_mu, L_Omega_mu));
  }
  
  // Set initial vote states
  vote[:, 1] ~ multi_normal_cholesky(vote0, diag_pre_multiply(tau_vote, L_Omega_vote));
  
  // Multivariate normal prior for true states/polling average for each party
  for (i in 2:T) {
    vote[:, i] ~ multi_normal_cholesky(vote[:, i-1], diag_pre_multiply(tau_vote, L_Omega_vote));
  }
  
  // Observations are noisy measurements of state with pollster bias added
  for (n in 1:N) {
    int party = j[n];
    int pollster = k[n];
    int time = t[n];
    vi[n] ~ normal(vote[party,time] + mu[party,pollster], sigma_obs[party]);
  }
}

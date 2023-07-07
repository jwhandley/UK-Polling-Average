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
  vector<lower=0>[J] sigma_state; // State noise
  vector<lower=0>[J] sigma_obs; // Observation noise
  matrix[J,K] mu; // Pollster effect
}

model {
  // Priors
  sigma_state ~ cauchy(0, 2.5);
  sigma_obs ~ cauchy(0, 2.5);
  
  // Standard normal prior for pollster effect for each party
  for (i in 1:J) {
    for (l in 1:K) {
      mu[i,l] ~ normal(0, 1);
    }
  }
  
  // True polling average follows a random walk for each party
  for (i in 1:J) {
    vote[i,1] ~ normal(vote0[i],sigma_state[i]);
    for (n in 2:T) {
      vote[i,n] ~ normal(vote[i,n-1],sigma_state[i]);
    }
  }
  
  // Observations are noisy measurments of state with pollster bias added
  for (n in 1:N) {
    int party = j[n];
    int pollster = k[n];
    int time = t[n];
    vi[n] ~ normal(vote[party,time] + mu[party,pollster], sigma_obs[party]);
  }
}

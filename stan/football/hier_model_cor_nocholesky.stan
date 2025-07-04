// A correlated hierchical model w/o Cholesky decomposition
// This model is identical to the hier_model_cor.stan, but without the Cholesky decomposition
// The Cholesky decomposition makes it easier to sample from the posterior but this model is easier to understand.
data {
  int<lower=0> nt; // number of teams
  int<lower=0> ng; // number of games
  array[ng] int<lower=0> ht; // home team index
  array[ng] int<lower=0> at; // away team index
  array[ng] int<lower=0> s1; // score home team
  array[ng] int<lower=0> s2; // score away team
}

parameters {
  matrix[2, nt] A; // attack / defence ability of each team
  real<lower=0> sigma_att; // standard deviation of attack
  real<lower=0> sigma_def; // standard deviation of defense
  real rho; // correlation between attack and defense
  real home; // home advantage
}

transformed parameters {
  // Covariance matrix for attack and defense
  matrix[2, 2] K;
  K[1, 1] = square(sigma_att);
  K[2, 2] = square(sigma_def);
  K[1, 2] = sigma_att * sigma_def * rho;
  K[2, 1] = K[1, 2];

  array[ng] real theta1; // score probability of home team
  array[ng] real theta2; // score probability of away team
  
  // Calculate the probabilities
  for (i in 1:ng) {
    theta1[i] = exp(home + A[1, ht[i]] - A[2, at[i]]);
    theta2[i] = exp(A[1, at[i]] - A[2, ht[i]]);
  }
}

model {
  // Priors
  for (t in 1:nt) {
    A[, t] ~ multi_normal(rep_vector(0, 2), K); // Multivariate normal for attack/defense abilities
  }
  home ~ normal(0,1);

  // Hyper priors
  sigma_att ~ exponential(1.0);   // Hyper prior for the spread in attack
  sigma_def ~ exponential(1.0);   // Hyper prior for the spread in defense
  rho ~ uniform(-1, 1);           // Prior for the correlation

  // Likelihood
  s1 ~ poisson(theta1);
  s2 ~ poisson(theta2);
}

generated quantities {
  // Calculate the log likelihood for the observed data
  array[ng] real log_lik;
  for (n in 1:ng) {
    log_lik[n] = poisson_lpmf(s1[n] | theta1[n]) + poisson_lpmf(s2[n] | theta2[n]);
  }  
}
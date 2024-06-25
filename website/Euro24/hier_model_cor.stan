// A correlated hierchical model w/ Cholesky decomposition
// The Cholesky decomposition makes it easier to sample from the posterior but this model is easier to understand.
data {
  int<lower=0> nt; // number of teams
  int<lower=0> ng; // number of games
  array[ng] int<lower=0> ht; // home team index
  array[ng] int<lower=0> at; // away team index
  array[ng] int<lower=0> s1; // score home team
  array[ng] int<lower=0> s2; // score away team
  
  int<lower=0> np; // number of predicted games if zero, no prediction
  array[np] int<lower=0> htnew; // home team index for prediction
  array[np] int<lower=0> atnew; // away team index for prediction
  array[np] int<lower=0> s1new; // score home team for prediction
  array[np] int<lower=0> s2new; // score away team for prediction
}

parameters {
  // correlation
  cholesky_factor_corr[2] L_u; // Attack and Defense correlation
  vector<lower=0>[2] sigma_u;  // Attack and Defense spread
  real home; // home advantage
  matrix[2, nt] A_z; // attack / defence ability of each team
}

transformed parameters {
  array[ng] real theta1; // score probability of home team
  array[ng] real theta2; // score probability of away team

  matrix[2, nt] A;
  A = diag_pre_multiply(sigma_u, L_u) * A_z; 

  for (i in 1:ng) {
    theta1[i] = exp(A[1, ht[i]] - A[2, at[i]]);
    theta2[i] = exp(A[1, at[i]] - A[2, ht[i]]);
  }
}

model {
  // hyper priors
  L_u ~ lkj_corr_cholesky(1.5); // Hyper prior for the correlation (rather weak)
  sigma_u ~ exponential(1.0);   // Hyper prior for the spread in attack and defense 
  // priors
  to_vector(A_z) ~ normal(0, 1); 
  // likelihood
  s1 ~ poisson(theta1);
  s2 ~ poisson(theta2);
}

generated quantities {
  // generate predictions
  array[np] real theta1new; // score probability of home team
  array[np] real theta2new; // score probability of away team
  //array[np] real s1new; // predicted score
  //array[np] real s2new; // predicted score
  array[ng] real log_lik;
  real log_lik_pred;
  
  for (i in 1:np) {
    theta1new[i] = exp(A[1, htnew[i]] - A[2, atnew[i]]);
    theta2new[i] = exp(A[1, atnew[i]] - A[2, htnew[i]]);
    //s1new[i] = poisson_rng(theta1new);
    //s2new[i] = poisson_rng(theta2new);
  }
  
  // log likelihood on the training data
  for (n in 1:ng) {
    log_lik[n] = poisson_lpmf(s1[n] | theta1[n]) + poisson_lpmf(s2[n] | theta2[n]);
  }
  
  // log likelihood on the prediction data
  log_lik_pred = 0;
  for (n in 1:np) {
    log_lik_pred += poisson_lpmf(s1new[n] | theta1new[n]) + poisson_lpmf(s2new[n] | theta2new[n]);
  }
  log_lik_pred /= np;
  
  
}
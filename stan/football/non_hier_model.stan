// Non hierarchical model for football prediction
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
  real home; // home advantage
  
  // See paper https://discovery.ucl.ac.uk/id/eprint/16040/1/16040.pdf 
  // Suggest using a sum-to-zero constraint: on average the attack capabilities should be zero
  // We model them using nt-1 parameters
  // See https://mc-stan.org/docs/2_18/stan-users-guide/parameterizing-centered-vectors.html
  vector[nt - 1] att_raw; // attack ability of each team
  vector[nt - 1] def_raw; // defence ability of each team
}

transformed parameters {
  vector[ng] theta1; // score probability of home team
  vector[ng] theta2; // score probability of away team

  // The last component is the sum of all others
  vector[nt] att = append_row(att_raw, -sum(att_raw));
  vector[nt] def = append_row(def_raw, -sum(def_raw));
  
  theta1 = exp(home + att[ht] - def[at]);
  theta2 = exp(att[at] - def[ht]);
}

model {
  // priors
  att ~ normal(0, 2);
  def ~ normal(0, 2);
  home ~ normal(0, 1);

  // likelihood
  s1 ~ poisson(theta1);
  s2 ~ poisson(theta2);
}

generated quantities {
  // generate predictions
  vector[np] theta1new; // score probability of home team
  vector[np] theta2new; // score probability of away team
  //array[np] real s1new; // predicted score
  //array[np] real s2new; // predicted score
  vector[ng] log_lik;
  real log_lik_pred;
  
  theta1new = exp(home + att[htnew] - def[atnew]);
  theta2new = exp(att[atnew] - def[htnew]);
  //s1new = poisson_rng(theta1new);
  //s2new = poisson_rng(theta2new);
  
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
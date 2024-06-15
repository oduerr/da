data {
  int<lower=0> nt; // number of teams
  int<lower=0> ng; // number of games
  array[ng] int<lower=0> ht; // home team index
  array[ng] int<lower=0> at; // away team index
  array[ng] int<lower=0> s1; // score home team
  array[ng] int<lower=0> s2; // score away team
  int<lower=0> np; // number of predicted games
  array[np] int<lower=0> htnew; // home team index for prediction
  array[np] int<lower=0> atnew; // away team index for prediction
}

parameters {
  real home; // home advantage
  // See paper https://discovery.ucl.ac.uk/id/eprint/16040/1/16040.pdf using a sum-to-zero constraint
  vector[nt - 1] att_raw; // attack ability of each team
  vector[nt - 1] def_raw; // defence ability of each team
  // hyper parameters
  real mu_att;
  real<lower=0> tau_att;
  real mu_def;
  real<lower=0> tau_def;
}

transformed parameters {
  array[ng] real theta1; // score probability of home team
  array[ng] real theta2; // score probability of away team

  // See https://mc-stan.org/docs/2_18/stan-users-guide/parameterizing-centered-vectors.html
  vector[nt] att = append_row(att_raw, -sum(att_raw));
  vector[nt] def = append_row(def_raw, -sum(def_raw));
  
  for (i in 1:ng) {
    theta1[i] = exp(home + att[ht[i]] - def[at[i]]);
    theta2[i] = exp(att[at[i]] - def[ht[i]]);
  }
}

model {
  // hyper priors
  mu_att ~ normal(0, 0.1);
  tau_att ~ normal(0, 1);
  mu_def ~ normal(0, 0.1);
  tau_def ~ normal(0, 1);

  // priors
  att ~ normal(mu_att, tau_att);
  def ~ normal(mu_def, tau_def);
  home ~ normal(0, 1);

  // likelihood
  s1 ~ poisson(theta1);
  s2 ~ poisson(theta2);
}

generated quantities {
  array[np] real theta1new; // score probability of home team
  array[np] real theta2new; // score probability of away team
  array[np] real s1new; // predicted score
  array[np] real s2new; // predicted score
  array[ng] real log_lik;

  for (i in 1:np) {
    theta1new[i] = exp(home + att[htnew[i]] - def[atnew[i]]);
    theta2new[i] = exp(att[atnew[i]] - def[htnew[i]]);
    s1new[i] = poisson_rng(theta1new[i]);
    s2new[i] = poisson_rng(theta2new[i]);
  }
  
  for (n in 1:ng) {
    log_lik[n] = poisson_lpmf(s1[n] | theta1[n]) + poisson_lpmf(s2[n] | theta2[n]);
  }
}
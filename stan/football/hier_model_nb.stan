data {
  int<lower=0> nt; //number of teams
  int<lower=0> ng; //number of games
  int<lower=0> ht[ng]; //home team index
  int<lower=0> at[ng]; //away team index
  int<lower=0> s1[ng]; //score home team
  int<lower=0> s2[ng]; //score away team
  int<lower=0> np; //number of predicted games
  int<lower=0> htnew[np]; //home team index for prediction
  int<lower=0> atnew[np]; //away team index for prediction
}

parameters {
  real home; //home advantage
  //See paper https://discovery.ucl.ac.uk/id/eprint/16040/1/16040.pdf using a sum-to-zero constraint
  vector[nt - 1] att_raw; //attack ability of each team
  vector[nt - 1] def_raw; //defence ability of each team
  //hyper parameters
  real mu_att;
  real<lower=0> tau_att;
  real mu_def;
  real<lower=0> tau_def;
  
  //Negtaive Binomonal
  real<lower=0> phi_1;
  real<lower=0> phi_2;
}

transformed parameters {
  vector[ng] theta1; //score probability of home team
  vector[ng] theta2; //score probability of away team

  //See https://mc-stan.org/docs/2_18/stan-users-guide/parameterizing-centered-vectors.html
  vector[nt] att = append_row(att_raw, -sum(att_raw));
  vector[nt] def = append_row(def_raw, -sum(def_raw));
  
  theta1 = exp(home + att[ht] - def[at]);
  theta2 = exp(att[at] - def[ht]);

}

model {
//hyper priors
mu_att ~ normal(0,0.1);
tau_att ~ normal(0,1);
mu_def ~ normal(0,0.1);
tau_def ~ normal(0,1);

//priors
att ~ normal(mu_att, tau_att);
def ~ normal(mu_def, tau_def);
home ~ normal(0,1);
phi_1 ~ normal(0,5);
phi_2 ~ normal(0,5);

//likelihood
    s1 ~ poisson(theta1);
    s2 ~ poisson(theta2);
}

generated quantities {
//generate predictions
  vector[np] theta1new; //score probability of home team
  vector[np] theta2new; //score probability of away team
  real s1new[np]; //predicted score
  real s2new[np]; //predicted score


  theta1new = exp(home + att[htnew] - def[atnew]);
  theta2new = exp(att[atnew] - def[htnew]);

  s1new = poisson_rng(theta1new);
  s2new = poisson_rng(theta2new);
}


//Correlation model, we assume that the attack and defense capability 
//of each team comes from a multivariate Gaussian, centered around zero. 
//We don't have to enforce the sum to zero constrain in this parametrization
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
  //correlation
  cholesky_factor_corr[2] L_u; //Attack and Defense correlation
  vector<lower=0>[2] sigma_u;  //Attack and Defense spread
  real home; //home advantage
  matrix[2, nt] A_z; //attack / defence ability of each team
}

transformed parameters {
  vector[ng] theta1; //score probability of home team
  vector[ng] theta2; //score probability of away team

  matrix[2,nt] A;
  A = diag_pre_multiply(sigma_u, L_u) * A_z; //
  for (i in 1:ng) {
    theta1[i] = exp(home + A[1, ht[i]] - A[2, at[i]]);
    theta2[i] = exp(A[1,at[i]] - A[2,ht[i]]);
  }
}

model {
  //hyper priors
    L_u ~ lkj_corr_cholesky(1.5); //Hyper prior for the correlation (rather weak)
    sigma_u ~ exponential(1.0);   //Hyper prior for the spread in attack and defense 
  //priors
    to_vector(A_z) ~ normal(0, 1); 
    home ~ normal(0,1);
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
  vector[ng] log_lik;
  
  for (i in 1:np) {
    theta1new[i] = exp(home + A[1, htnew[i]] - A[2, atnew[i]]);
    theta2new[i] = exp(A[1,atnew[i]] - A[2,htnew[i]]);
  }
  s1new = poisson_rng(theta1new);
  s2new = poisson_rng(theta2new);
  
  for (n in 1:ng){
    log_lik[n] = poisson_lpmf(s1[n] | theta1) + poisson_lpmf(s2[n] | theta2);
  }
}


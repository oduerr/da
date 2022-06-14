data {
  int<lower=1> N;            //number of observations (in training)
  real y[N];                 //outcomes (in training)
  real<lower=0> x[N];        //x (living space)
  // grouping factors
  int<lower=1> J;                   //number of subjects
  int<lower=1,upper=J> j[N];        //subject id
  
  //Test data
  int<lower=1> N_t; 
  real<lower=0> y_t[N_t];   
  real<lower=0> x_t[N_t];   
  int<lower=1,upper=J> j_t[N_t];
}

parameters {
  real<lower=0> sigma_e;               // residual std
  matrix[2,J] u;                       // city level intercept and slope
}


model {
  real mu; // conditional mean of the dependent variable

  
  //prior 
  u[,1] ~ normal(100,200); //Population spread of intercept
  u[,2] ~ normal(1000,1000);  //Population spread of slope (Euro / sqm)
  
  //priors
  sigma_e ~ normal(0, 100);        // prior for residual standard deviation +- 100 Euro
 
  //likelihood
  for (i in 1:N){
    mu = u[1,j[i]] + (u[2,j[i]])*x[i];
    y[i] ~ normal(mu, sigma_e);
  }
}

generated quantities {
  vector[N] log_lik;
  vector[N_t] log_lik_t;
  vector[N_t] y_t_pred;
  
  for (n in 1:N){
    log_lik[n] = normal_lpdf(y[n] |   u[1,j[n]] + ( u[2,j[n]])*x[n], sigma_e);
  }
  
  for (n in 1:N_t){
    log_lik_t[n] = normal_lpdf(y_t[n] | u[1,j[n]] + (u[2,j[n]])*x[n], sigma_e);
    y_t_pred[n] = normal_rng( u[1,j[n]] + (u[2,j[n]])*x[n], sigma_e);
  }
}


data {
  int<lower=1> N;            //number of observations (in training)
  real y[N];                 //outcomes (in training)
  real x[N];                 //x (living space)
  // grouping factors
  int<lower=1> J;                   //number of subjects
  int<lower=1,upper=J> j[N];        //subject id
  
  //Test data
  int<lower=1> N_t; 
  real y_t[N_t];   
  real x_t[N_t];   
  int<lower=1,upper=J> j_t[N_t];
}

parameters {
  real<lower=0> sigma_e;               // residual std
  matrix[2,J] u;                       // City level intercept (1) and slope (2)
  vector[2] pu;                        // County level intercept and slope (mean)
  vector[2] ps;                        // County level intercept and slope (sd)
}

model {
  real mu; 
  //Hyperprior
  //Spread to the means
  pu ~ normal(0, 1);    //Intercept and slope mean
  //Uncertainty the means
  ps ~ exponential(1.);
  //prior 
  u[1,] ~ normal(pu[1],ps[1]);  //Intercept for the individual cities
  u[2,] ~ normal(pu[2],ps[2]);  //Slope for the individual cities
  //priors
  sigma_e ~ exponential(1.);        // prior for residual standard deviation
 
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


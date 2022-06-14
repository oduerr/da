data {
  int<lower=1> N;            //number of observations (in training)
  real y[N];                 //outcomes (in training)
  real x[N];        //x (living space)
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
  vector[2] beta;                      // fixed-effects parameters
  real<lower=0> sigma_e;               // residual std
  matrix[2,J] z_u;                     // city level intercept and slope (z-values)
  cholesky_factor_corr[2] L_u;         // County level intercept and slope (Correlation) (not covariance!)
  vector<lower=0>[2] sigma_u;          // County standard deviations sigma_u L_u is a matrix which 
}

transformed parameters {
  // this transform random effects so that they have the correlation
  // matrix specified by the correlation matrix above
  matrix[2,J] u;
  u = diag_pre_multiply(sigma_u, L_u) * z_u; //
}

model {
  real mu; // conditional mean of the dependent variable

  //Hyperprior: 
  //    Correlation Matrix
  L_u ~ lkj_corr_cholesky(1.5); // LKJ prior for the correlation matrix
  //    Stderr
  sigma_u ~ exponential(1.0e-2);
  //    Means
  beta[1] ~ normal(0, 100); 
  beta[2] ~ normal(5, 10); 
  
  //Priors:
  to_vector(z_u) ~ normal(0,1);  // 
  sigma_e ~ exponential(1.0e-2);       // prior for residual standard deviation +- 100 Euro
 
  //likelihood
  for (i in 1:N){
    mu = beta[1] + u[1,j[i]] + (beta[2] + u[2,j[i]])*x[i];
    y[i] ~ normal(mu, sigma_e);
  }
}

generated quantities {
  vector[N] log_lik;
  vector[N_t] log_lik_t;
  vector[N_t] y_t_pred;
  
  for (i in 1:N){
    log_lik[i] = normal_lpdf(y[i] |   beta[1] + u[1,j[i]] + (beta[2] + u[2,j[i]])*x[i], sigma_e);
  }
  
  for (i in 1:N_t){
    log_lik_t[i] = normal_lpdf(y_t[i] | beta[1] + u[1,j[i]] + (beta[2] + u[2,j[i]])*x[i], sigma_e);
    y_t_pred[i] = normal_rng(beta[1] + u[1,j[i]] + (beta[2] + u[2,j[i]])*x[i], sigma_e);
  }
}


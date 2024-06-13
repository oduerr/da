// Complete Pooling
// Modelling with complete pooling (all cities share the same intercept and slope)
data {
  int<lower=1> N;            // number of observations (in training)
  array[N] real y;           // outcomes (in training)
  array[N] real x;           // x (living space)
  int<lower=1> J;            // number of subjects
  array[N] int<lower=1, upper=J> j; // subject id
  
  int<lower=1> N_t; 
  array[N_t] real y_t;   
  array[N_t] real x_t;   
  array[N_t] int<lower=1, upper=J> j_t;
}

parameters {
  real<lower=0> sigma_e;     // residual std
  real alpha;                // Shared intercept
  real beta;                 // Shared slope
}

model {
  real mu;

  // Priors for the shared parameters
  alpha ~ normal(0, 1);  // Shared intercept
  beta ~ normal(0, 1);   // Shared slope
  sigma_e ~ exponential(1.0);   // Prior for residual standard deviation

  // Likelihood
  for (i in 1:N) {
    mu = alpha + beta * x[i];
    y[i] ~ normal(mu, sigma_e + 1e-8);
  }
}

generated quantities {
  array[N] real log_lik;
  array[N_t] real log_lik_t;
  array[N_t] real y_t_pred;
  
  for (n in 1:N) {
    log_lik[n] = normal_lpdf(y[n] | alpha + beta * x[n], sigma_e);
  }
  
  for (n in 1:N_t) {
    log_lik_t[n] = normal_lpdf(y_t[n] | alpha + beta * x_t[n], sigma_e);
    y_t_pred[n] = normal_rng(alpha + beta * x_t[n], sigma_e);
  }
}
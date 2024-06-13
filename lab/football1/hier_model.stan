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
  array[J, 2] real u;        // City level intercept (1) and slope (2)
  vector[2] pu;              // County level intercept and slope (mean)
  vector<lower=0>[2] ps;     // County level intercept and slope (sd)
}

model {
  real mu;

  // Hyperpriors
  pu ~ normal(0, 1);          // Intercept and slope mean
  ps ~ exponential(1.0);      // Uncertainty of the means

  // Priors for city-level parameters
  u[:, 1] ~ normal(pu[1], ps[1]);  // Intercept for the individual cities
  u[:, 2] ~ normal(pu[2], ps[2]);  // Slope for the individual cities
  sigma_e ~ exponential(1.0);   // Prior for residual standard deviation

  // Likelihood
  for (i in 1:N) {
    mu = u[j[i], 1] + u[j[i], 2] * x[i];
    y[i] ~ normal(mu, sigma_e);
  }
}

generated quantities {
  array[N] real log_lik;
  array[N_t] real log_lik_t;
  array[N_t] real y_t_pred;
  
  for (n in 1:N) {
    log_lik[n] = normal_lpdf(y[n] | u[j[n], 1] + u[j[n], 2] * x[n], sigma_e);
  }
  
  for (n in 1:N_t) {
    log_lik_t[n] = normal_lpdf(y_t[n] | u[j_t[n], 1] + u[j_t[n], 2] * x_t[n], sigma_e);
    y_t_pred[n] = normal_rng(u[j_t[n], 1] + u[j_t[n], 2] * x_t[n], sigma_e);
  }
}
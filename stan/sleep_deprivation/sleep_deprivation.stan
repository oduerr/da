data {
  int<lower=1> N;            //number of observations
  real RT[N];                //reaction times

  int<lower=0,upper=9> Days[N];   //y, predictor (days of sleep deprivation)

  // grouping factors
  int<lower=1> J;                   //number of subjects
  int<lower=1,upper=J> Subject[N];  //subject id
}

parameters {
  vector[2] beta;                   // population intercept and slope
  real<lower=0> sigma_e;            // residual std
  matrix[2,J] u;                    // random effect matrix
}


model {
  real mu; // conditional mean of the dependent variable

  //hyper_prior
  to_vector(u) ~ normal(0,2);
  
  //priors
  sigma_e ~ normal(0, 5);       // prior for residual standard deviation
  beta[1] ~ normal(0.3, 0.5);   // prior for fixed-effect intercept
  beta[2] ~ normal(0.2, 2);     // prior for fixed-effect slope

  //likelihood
  for (i in 1:N){
    mu = beta[1] + u[1,Subject[i]] + (beta[2] + u[2,Subject[i]])*Days[i];
    RT[i] ~ normal(mu, sigma_e);
  }
}

generated quantities {
  vector[N] log_lik;
  for (n in 1:N){
    log_lik[n] = normal_lpdf(RT[n] |  beta[1] + u[1,Subject[n]] + (beta[2] + u[2,Subject[n]])*Days[n], sigma_e);
  }
}


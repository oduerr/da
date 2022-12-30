data {
  int<lower=1> N;            //number of observations
  real RT[N];                //reaction times

  int<lower=0,upper=9> Days[N];   //y, predictor (days of sleep deprivation)

  // grouping factors
  int<lower=1> J;                   //number of subjects
  int<lower=1,upper=J> Subject[N];  //subject id
}

parameters {
  vector[2] beta;                   // fixed-effects parameters
  real<lower=0> sigma_e;            // residual std
  vector<lower=0>[2] sigma_u;       // random effects standard deviations

  // declare L_u to be the Choleski factor of a 2x2 correlation matrix
  cholesky_factor_corr[2] L_u;

  matrix[2,J] z_u;                  // random effect matrix
}

transformed parameters {
  // this transform random effects so that they have the correlation
  // matrix specified by the correlation matrix above
  matrix[2,J] u;
  u = diag_pre_multiply(sigma_u, L_u) * z_u;
}

model {
  real mu; // conditional mean of the dependent variable

  //priors
  L_u ~ lkj_corr_cholesky(1.5); // LKJ prior for the correlation matrix
  to_vector(z_u) ~ normal(0,2);
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
  matrix[2, 2] Omega;
  vector[N] log_lik;
  
  Omega = L_u * L_u'; // so that it return the correlation matrix
  for (n in 1:N){
    log_lik[n] = normal_lpdf(RT[n] |  beta[1] + u[1,Subject[n]] + (beta[2] + u[2,Subject[n]])*Days[n], sigma_e);
  }
}



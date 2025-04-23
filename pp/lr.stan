data {
  int<lower=0> N;          // number of observations
  vector[N] x;             // predictor
  vector[N] y;             // response
}

parameters {
  real beta_0;             // intercept
  real beta_1;             // slope
  real<lower=0> sigma;     // standard deviation
}

model {
  vector[N] y_hat;

  // linear predictor
  y_hat = beta_0 + beta_1 * x;

  // likelihood
  y ~ normal(y_hat, sigma);
}
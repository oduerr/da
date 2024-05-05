data {
  int<lower=0> N;           // Number of data points
  vector[N] x;              // Predictor variable
  vector[N] y;              // Outcome variable
}

parameters {
  real beta_0;              // Intercept
  real beta_1;              // Slope
  real<lower=0> sigma;      // Standard deviation of the residuals
}

model {
    y ~ normal(beta_1 * x + beta_0, sigma);
}





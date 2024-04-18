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
    // Adding the likelihood of the data given the parameters
    for (i in 1:N) {
        target += normal_lpdf(y[i] | beta_1 * x[i] + beta_0, sigma);
    }
}



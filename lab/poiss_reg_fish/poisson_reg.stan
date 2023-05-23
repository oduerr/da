data {
  int<lower=0> N;          // number of observations
  int<lower=0> K;          // number of predictors
  matrix[N, K] X;          // predictor matrix
  int<lower=0> y[N];       // response variable
}
parameters {
  vector[K] beta;          // coefficients for predictors
  real<lower=0> alpha;     // intercept
}
model {
 y ~ poisson(exp(X * beta + alpha)); // Poisson regression model
 beta ~ normal(0,10);
 alpha ~ normal(0,10);
}
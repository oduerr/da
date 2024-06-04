data {
  int<lower=0> N;                   // Number of data points
  vector[N] x;                      // Predictor variable
  array[N] int<lower=0, upper=1> y; // Outcome variable (binary, 0 or 1)
  //For the posterior checks
  int<lower=0> N2;
  vector[N2] x2;
}
parameters {
  real alpha;              // Intercept
  real beta;               // Slope
}
model {
  y ~ bernoulli_logit(alpha + beta * x);     
  //No priors on alpha and beta
  //Alternative version (not so much optimized)
  //y ~ bernoulli(inv_logit(alpha + beta * x)); //inv_logit aka sigmoid
}
// Generate quantities of interest
generated quantities {
  vector[N2] p_predict;
  p_predict = inv_logit(alpha + beta * x2);
}


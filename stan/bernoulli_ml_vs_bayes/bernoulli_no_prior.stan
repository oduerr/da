data {
  int<lower=0> N;          // Number of data points
  array[N] int<lower=0, upper=1> y; // Outcome variable (binary, 0 or 1)
}
parameters {
  real alpha;              // Intercept
  real beta;               // Slope
}
model {
  y ~ bernoulli_logit(alpha + beta * x); 
  alpha ~ normal(0,1.5);
  beta ~ normal(0,1.5);
  //Alternative version (not so much optimized)
  //y ~ bernoulli(inv_logit(alpha + beta * x)); //inv_logit aka sigmoid
}
// Generate quantities of interest
generated quantities {
  vector[N2] p_predict;
  p_predict = inv_logit(alpha + beta * x2);
}


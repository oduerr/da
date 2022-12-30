//Example for logistic regression using rstan
data {
  int<lower=0> N;
  vector[N] x;
  int<lower=0,upper=1> y[N];
}
parameters {
  real alpha;
  real beta;
}
model {
  alpha ~ normal(0,10);
  beta ~ normal(0,10);
  y ~ bernoulli(inv_logit(alpha + beta * x)); //inv_logit aka sigmoid
}

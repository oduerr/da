//Example for logistic regression using rstan
data {
  int<lower=0> N;
  vector[N] x;
  int<lower=0,upper=1> y[N];
  //For the posterior checks
  int<lower=0> N2;
  vector[N2] x2;
}
parameters {
  real alpha;
  real beta;
}
model {
  y ~ bernoulli_logit(alpha + beta * x); 
  //No priors on alpha and beta
  //Alternative version (not so much optimized)
  //y ~ bernoulli(inv_logit(alpha + beta * x)); //inv_logit aka sigmoid
}
generated quantities {
  real Y_predict[N2];
  vector[N2] p_predict;
  Y_predict = bernoulli_logit_rng(alpha + beta * x2);
  p_predict = inv_logit(alpha + beta * x2);
}

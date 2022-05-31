data {
  int<lower=0> J; // number of schools
  vector[J] y; // estimated treatment effect (school j)
  vector<lower=0> [J] sigma; // std err of effect estimate (school j)
}
parameters {
  vector[J]  theta;
  real<lower=0> tau;
  real mu;
}
model {
  mu ~ normal(0,5);
  tau ~ cauchy(0,5);
  theta ~ normal(mu, tau);
  y ~ normal(theta, sigma);
}
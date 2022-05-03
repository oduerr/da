data {
  int<lower=1> N;
  vector[N] height;
}
parameters {
  real mu;
  real<lower=0,upper=50> sigma;
}
model {
  height ~ normal(mu, sigma);
  sigma ~ uniform(0, 50);
  mu ~ normal(178, 20);
}

data {
  int<lower=1> N;
  real hbar;
  vector[N] height;
  vector[N] weight;
}
parameters {
  real <lower=0, upper=50> sigma;
  real<lower=0> b;
  real a;
}
model {
  vector[N] mu;
  mu = a + b * (height - hbar);
  weight ~ normal(mu, sigma);
  a ~ normal(178, 20);
  b ~ lognormal(0, 1);
  sigma ~ uniform(0, 50);
}
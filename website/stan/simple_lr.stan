data{
  int<lower=0> N;
  vector[N] y;
  vector[N] x;
}

parameters{
  real a; //Instead of using e.g. half Gaussian
  real b;
  real<lower=0> sigma;
}

model{
  //y ~ normal(mu, sigma);
  y ~ normal(a * x + b, sigma);
  a ~ normal(3, 10); 
  b ~ normal(0, 10); 
  sigma ~ normal(0,10);
}

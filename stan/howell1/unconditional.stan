data {
  int<lower=1> N;
  vector[N] height; 
}
parameters {
  real mu;
  real sigma;
}
model {
  height ~ normal(mu, sigma);
  sigma ~ uniform(0, 50);
  mu ~ normal(178, 20);
}
generated quantities {
  real pred_height = normal_rng(mu, sigma);
}




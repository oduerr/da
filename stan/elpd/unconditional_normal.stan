data {
  int<lower=0> N;
  vector[N] y;
}

parameters {
  real mu; 
}

model {
  y ~ normal(mu, 1.0);
  mu ~ normal(0, 10);
}

generated quantities {
  vector[N] log_lik;
  for (n in 1:N) {
    log_lik[n] = normal_lpdf(y[n] | mu, 1.0);
  }
}

data{
  int<lower=0> N;
  vector[N] y;
  vector[N] x;
}

parameters{
  real a; 
  real b;
  real<lower=0> sigma;
  real<lower=1> nu;
}

model{
  y ~ student_t(nu, a * x + b, sigma);
  a ~ normal(0, 10); 
  b ~ normal(0, 10); 
  nu ~ normal(1,10);
  sigma ~ normal(0, 10); 
}

generated quantities {
  vector[N] log_lik;
  for (n in 1:N) {
    log_lik[n] = student_t_lpdf(y[n] | nu, a * x[n] + b, sigma);
  }
}
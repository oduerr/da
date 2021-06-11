//https://mc-stan.org/docs/2_19/stan-users-guide/gaussian-process-regression.html
data {
  int<lower=1> N;
  real x[N];
}
// Creation of m and K
transformed data {
  vector[N] m = rep_vector(0, N); //The mean function 
  matrix[N, N] K = cov_exp_quad(x, 1.0, 1.0); //The covariate
  for (n in 1:N) K[n, n] = K[n, n] + 0.001; //Numerical stability
}
parameters {
  vector[N] f;
}
// Sampling from m and K
model {
  f ~ multi_normal(m, K);
}



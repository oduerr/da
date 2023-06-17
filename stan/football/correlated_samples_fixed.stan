data {
  int<lower=0> K;
  vector[K] sigma;
  matrix[K,K] rho;
  int<lower=0> J;
}
transformed data{
  vector[K] mu = rep_vector(0, K);
}
generated quantities {
  matrix[J,2] samples;
  matrix[K, K] Sigma;
  Sigma = quad_form_diag(rho, sigma);
  for (j in 1:J) {
    samples[j] = multi_normal_rng(mu, Sigma)';
  }
}

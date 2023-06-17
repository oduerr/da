data {
  int<lower=0> K;
  vector[K] sigma;
  matrix[K,K] rho;
  int<lower=0> J;
}
transformed data{
  vector[K] mu = rep_vector(0, K);
}
parameters{
  cholesky_factor_corr[2] L_u; 
}
model {
  L_u ~ lkj_corr_cholesky(1.5);;
}
generated quantities {
  matrix[J,2] samples;
  matrix[K, K] Sigma;
  
  matrix[J,2] samples2;
  matrix[J,2] Z_gen;
  
  //Generating sample by sampling from a Gaussian
  Sigma = quad_form_diag(rho, sigma);
  for (j in 1:J) {
    samples[j] = multi_normal_rng(mu, Sigma)';
  }
  
  //Generating samples using choleski decomposition
  for (j in 1:J){
    Z_gen[j,1] = normal_rng(0,1);
    Z_gen[j,2] = normal_rng(0,1);
  }
  samples2 = diag_pre_multiply(sigma, L_u) * Z_gen;
}

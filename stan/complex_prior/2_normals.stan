parameters {
  real theta;
}

model {
 
  // Calculate the log-probabilities for each Gaussian
  vector[2] log_probs;

  log_probs[1] = normal_lpdf(theta | -10, 1);
  log_probs[2] = normal_lpdf(theta | 12, 1);

  // Increment the log-probability with the log-sum-exp trick to avoid underflow
  target += log_sum_exp(log_probs);
}

parameters {
  real<lower=-11, upper=-9> theta1;
  real<lower=9, upper=11> theta2;
}

model {
  // The mixing proportions for the two Gaussians
  vector[2] mixing_proportions = rep_vector(0.5, 2);

  // Calculate the log-probabilities for each truncated Gaussian
  vector[2] log_probs;

  log_probs[1] = log(mixing_proportions[1]) + normal_lpdf(theta1 | -10, 1);
  log_probs[2] = log(mixing_proportions[2]) + normal_lpdf(theta2 | 10, 1);

  // Increment the log-probability with the log-sum-exp trick to avoid underflow
  target += log_sum_exp(log_probs);
}

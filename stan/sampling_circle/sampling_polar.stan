parameters {
  real<lower=0> r;      // Radius, must be non-negative
  real<lower=0, upper=2 * pi()> theta;  // Angle in radians, constrained to [0, 2*pi)
}

model {
  // Apply a penalty outside the angular range of 40 to 320 degrees
  // Normalize angle to be within 0 and 2*pi, although already constrained by parameter declaration
  real lower = 0.698; // 40 degrees in radians
  real upper = 5.585; // 320 degrees in radians

  // Penalize points outside this angular range
  if (theta < lower || theta > upper)
    target += -100; // Applying a strong negative log-likelihood penalty

  // Optionally, enforce the radius to be near 1
  r ~ normal(1, 0.001); // Radius should be close to 1
}

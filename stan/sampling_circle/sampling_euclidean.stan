parameters {
  vector[2] xy;  // Two-dimensional parameter for Cartesian coordinates
}

model {
  // Convert xy to polar coordinates
  real r = sqrt(dot_self(xy));
  real theta = atan2(xy[2], xy[1]); // Angle in radians

  // Apply a penalty outside the angular range of 40 to 320 degrees
  if (theta < 0) theta += 2 * pi(); // Normalize angle to be within 0 and 2*pi

  // Define the acceptable angular range in radians
  real lower = 0.698; // 40 degrees in radians
  real upper = 5.585; // 320 degrees in radians

  // Penalize points outside this angular range
  if (theta < lower || theta > upper)
    target += -100; // Applying a strong negative log-likelihood penalty

  // Optionally, you can also enforce the radius to be near 1
  r ~ normal(1, 0.01);
}

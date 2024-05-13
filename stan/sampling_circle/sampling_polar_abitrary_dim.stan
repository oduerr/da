data {
  int<lower=1> D;  // Dimensionality of the space
}

parameters {
  vector[D] x;  // Cartesian coordinates in D dimensions
}

transformed parameters {
  real r = sqrt(dot_self(x));  // Radius
}

model {
  // Enforce the radius to be near 1
  r ~ normal(1, 0.01);
}

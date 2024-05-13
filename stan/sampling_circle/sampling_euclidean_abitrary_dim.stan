data {
  int<lower=1> D;  // Dimensionality of the space
}

parameters {
  vector[D] x;  // Cartesian coordinates in D dimensions
}

model {
  // Enforce the radius to be near 1
  1 ~ normal(sqrt(dot_self(x)), 0.01);
}

generated quantities {
  real radius = sqrt(dot_self(x));
}
 

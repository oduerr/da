parameters {
  real theta;
}

model {
  //theta ~ normal(0, 1);
  target += 10+normal_lpdf(theta | 0, 1);
}

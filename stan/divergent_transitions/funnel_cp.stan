data {
  real w_par; //To control the width of the Funnel w/o recompiling just a paramter
  real y;     //A single data point
}
parameters {
  real v;  //The spread
  real mu; //The mean value
}
model {
  v ~ normal(0,w_par);
  mu ~ normal(0, exp(v));
  y ~ normal(mu, 1);
}
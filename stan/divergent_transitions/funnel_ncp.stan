data {
  real w_par; //To control the width of the Funnel w/o recompiling just a paramter
  real y;     //A single data point
}
parameters {
  real v; //The spread
  real mu_tilde; //The N(0,1) variable of the mean value
}
transformed parameters{
  real mu = exp(v) * mu_tilde;
}
model {
  v ~ normal(0, w_par);
  mu_tilde ~ normal(0, 1);
  y ~ normal(mu, 1);
}
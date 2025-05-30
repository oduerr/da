data {
  int<lower=1> N;
  vector[N] W;
  array int sex[N];
}
parameters {
  real sigma;
  vector[2] a;
}
transformed parameters{
  //Calculation of the mean for the weights
  vector[N] mu_i;   
  for ( i in 1:N ) {
    mu_i[i] = a[sex[i]];
  }
}
model {
  a ~ normal(60, 10);
  sigma ~ uniform(0, 10);
  W ~ normal(mu_i, sigma);
}
generated quantities{
  real diff_fm;
  diff_fm = a[1] - a[2];
}


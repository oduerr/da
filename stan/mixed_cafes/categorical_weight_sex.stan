data {
  int<lower=1> N;
  vector[N] W;
  int sex[N];
}
parameters {
  real sigma;
  vector[2] a;
}
model {
  //Calculation of the mean for the weights
  vector[N] mu_i;   
  a ~ normal(60, 10);
  for ( i in 1:N ) {
    mu_i[i] = a[sex[i]];
  }
  sigma ~ uniform(0, 10);
  W ~ normal(mu_i, sigma);
}


generated quantities{
  real diff_fm;
  diff_fm = a[1] - a[2];
}


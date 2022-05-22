data {
  int<lower=1> N;
  vector[N] H;
  vector[N] W;
  int sex[N];
}
parameters {
  real sigma;
  vector[2] a;
  vector[2] b;
}
model {
  vector[N] mu_i;   
  sigma ~ uniform(0, 10);
  
  //Calculation of the mean for the weights
  a ~ normal(60, 10);
  b ~ lognormal(0,1);
  for ( i in 1:N ) {
    mu_i[i] = a[sex[i]] + b[sex[i]] * H[i];
  }
  W ~ normal(mu_i, sigma);
}
generated quantities{
  real diff_fm;
  diff_fm = a[1] - a[2];
}
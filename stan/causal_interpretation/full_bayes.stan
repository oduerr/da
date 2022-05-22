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
  real sigma_H;
  vector[2] a_H;
}
model {
  vector[N] mu_i;   
  vector[N] mu_H_i;   
  
  //Calculation of the mean for the weights
  a ~ normal(60, 10);
  b ~ lognormal(0,1);
  a_H ~ normal(160, 10);
  for ( i in 1:N ) {
    mu_i[i] = a[sex[i]] + b[sex[i]] * H[i];
    mu_H_i[i] = a_H[sex[i]];
  }
  
  //Outcome Distribution(for W)
  sigma ~ uniform(0, 10);
  W ~ normal(mu_i, sigma);
  
  //Outcome Distribution(for H)
  sigma_H ~ uniform(0, 10);
  H ~ normal(mu_H_i, sigma_H);
}
generated quantities{
  real diff_fm;
  diff_fm = a[1] - a[2];
}
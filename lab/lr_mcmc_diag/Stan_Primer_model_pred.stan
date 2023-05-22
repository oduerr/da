//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

data{
  int<lower=0> N;
  vector[N] y;
  vector[N] x;
  //For the prediced distribution
  int<lower=0> N2;
  vector[N2] x2;
}

parameters{
  real a; //Instead of using e.g. half Gaussian
  real b;
  real <lower=0> sigma;
}

model{
  y ~ normal(a * x + b, sigma);
  a ~ normal(0, 10); 
  b ~ normal(0, 10); 
  sigma ~ normal(0,10);
}

generated quantities {
  real Y_predict[N2]; 
  real mu_predict[N2]; 
  for (i in 1:N2){
    mu_predict = normal_rng(a * x2 + b,0.000001);
    Y_predict = normal_rng(a * x2 + b, sigma);
  }
}






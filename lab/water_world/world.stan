//description of the data part
data {
  int<lower=0> N;
  int<lower=0> W;
}

//parameters, here fraction of the water
parameters {
  real<lower=0, upper=1> p;
}
 
//priors and likelihood
model {
  p ~ uniform(0.4, 1.0); //Prior for p
  W ~ binomial(N, p);    //Likelihood      
}


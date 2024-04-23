data {
  int<lower=0> N;                    
  array[N] real Temp;                
  array[N] int<lower=0, upper=1> Failure;   // failure status at each observation
}

parameters {
  real alpha;                         // intercept
  real beta;                          // coefficient for temperature
}

model {
  // Logistic function for probability calculation
  for (i in 1:N) {
    real p = 1 / (1 + exp(-(alpha + beta * Temp[i])));  // Logistic function
    Failure[i] ~ bernoulli(p);  // Using the Bernoulli distribution with probability p
  }
}
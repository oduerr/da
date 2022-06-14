data {
  int<lower=1> N;            //number of observations (in training)
  real y[N];                 //outcomes (in training)
  real x[N];                 //x (living space)
  // grouping factors
  int<lower=1> J;                   //number of subjects
  int<lower=1,upper=J> j[N];        //subject id
}

parameters {
  real<lower=0> sigma_e;               // residual std
  matrix[2,J] u;                       // city level intercept and slope
  vector[2] pu;                        // County level intercept and slope (mean)
  vector[2] ps;                        // County level intercept and slope (sd)
  real<lower=1> nu;
}


model {
  real mu; // conditional mean of the dependent variable

  //Hyperprior
  //Spread to the means
  pu ~ normal(0, 1); //Intercept 
  //Uncertainty the means
  ps ~ normal(0,1);

  //prior 
  u[1,] ~ normal(pu[1],ps[1]); //Population spread of intercept
  u[2,] ~ normal(pu[2],ps[2]);  //Population spread of slope (Euro / sqm)
  
  //priors
  sigma_e ~ normal(0, 1);        // prior for residual standard deviation +- 100 Euro
  nu ~ normal(1,10);
  
  //likelihood
  for (i in 1:N){
    mu = u[1,j[i]] + (u[2,j[i]])*x[i];
    y[i] ~ normal(mu, sigma_e);
    //y[i] ~ student_t(nu, mu, sigma_e);
  }
}

generated quantities {
  vector[N] log_lik;

  for (n in 1:N){
    log_lik[n] = normal_lpdf(y[n] |   u[1,j[n]] + ( u[2,j[n]])*x[n], sigma_e);
  }
  
}


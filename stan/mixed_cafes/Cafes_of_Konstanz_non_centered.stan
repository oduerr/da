// Modeling the weighting times in cafes
// Hierarchical Modelling 
// Example is motivated by the golem cafe example:  
data{
    int N;        //Total number of visits to cafes
    int n_cafes;  //Number of different cafe in konstanz
    vector[N] y;    //Waiting times for the vists
    array [N] int<lower=1> cafe;  //Number of the cafe for the different visits
}
parameters {
  real<lower=0> sigma;
  real mu_bar;
  real<lower=0> tau;  // group-level std dev
  vector[n_cafes] z;
}
transformed parameters {
  vector[n_cafes] mu = mu_bar + tau * z;
}
model {
  mu_bar ~ normal(10, 5);
  tau ~ normal(0, 5);
  z ~ normal(0, 1);
  y ~ normal(mu[cafe], sigma);
}





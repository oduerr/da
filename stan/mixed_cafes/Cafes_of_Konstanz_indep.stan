// Modeling the weighting times in cafes
// All Cafes are modeled independently (no pooling)
// Example is motivated by the golem cafe example:  
data{
    int<lower=1> N;        //Total number of visits to cafes
    int<lower=1> n_cafes;  //Number of different cafe in konstanz (here 5)
    vector[N] y;           //Waiting times for the vists
    array [N] int<lower=1> cafe;  //Number of the cafe for the different visits
}
parameters{
    vector<lower=0>[n_cafes] mu;  //Mean time in cafe
    real<lower=0> sigma;          //The spread assumed to be the same
}

model{
    // Defining the priors (no pooling)
    vector[N] mu_i;   
    for ( i in 1:n_cafes ) {
      mu[i] ~ uniform(1, 25); //Prior for cafes 1,2,3,...,n_cafe
    }
    sigma ~ cauchy(3, 2); 
    for ( i in 1:N ) {
        mu_i[i] = mu[cafe[i]]; //Prior for the visits
    }
    y ~ normal(mu_i, sigma);
}





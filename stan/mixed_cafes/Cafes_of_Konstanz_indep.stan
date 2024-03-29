//Modeling the weighting times in cafes
//Example is motivated by the golem cafe example:  
data{
    int<lower=1> N;        //Total number of visits to cafes
    int<lower=1> n_cafes;  //Number of different cafe in konstanz (here 5)
    real<lower=0> y[N];    //Waiting times for the vists
    int<lower=1> cafe[N];  //Number of cafe for the different visits
}
parameters{
    vector<lower=0>[n_cafes] mu;  //Mean time in cafe
    real<lower=0> sigma;          //The spread assumed to be the same
}

model{
    vector[N] mu_i;   
    for ( i in 1:n_cafes ) {
      mu[i] ~ uniform(1, 25);
    }
    sigma ~ cauchy(3, 2); 
    for ( i in 1:N ) {
        mu_i[i] = mu[cafe[i]];
    }
    y ~ normal(mu_i, sigma);
}





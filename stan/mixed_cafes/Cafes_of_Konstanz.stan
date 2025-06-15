// Modeling the weighting times in cafes
// Hierarchical Modelling 
// Example is motivated by the golem cafe example:  
data{
    int N;        //Total number of visits to cafes
    int n_cafes;  //Number of different cafe in konstanz
    vector[N] y;    //Waiting times for the vists
    array [N] int<lower=1> cafe;  //Number of the cafe for the different visits
}
parameters{
    vector<lower=0>[n_cafes] mu_delta;  //Derivation of the mean time from the cafe
    real<lower=0> mu_bar;        //The mean time in the cafe (Population)
    real<lower=0> sigma;         //The parameter sigma 
}
model{
    vector[N] mu_i;   
    sigma ~ cauchy(3, 2); 
    mu_bar ~ uniform(1, 25);
    mu_delta ~ normal( 0 , 5 );
    for ( i in 1:N ) {
        mu_i[i] = mu_delta[cafe[i]] + mu_bar;
    }
    y ~ normal(mu_i, sigma);
}

generated quantities {
    vector[n_cafes] mu;
    for (j in 1:n_cafes) {
        mu[j] = mu_bar + mu_delta[j];
    }
}



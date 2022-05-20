//Modeling the weighting times in cafes
//Example is motivated by the golem cafe example:  
//
data{
    int N;
    int n_cafes;
    real y[N];
    int cafe[N];
}
parameters{
    vector<lower=0>[n_cafes] z; //Derivation of the mean time from the cafe
    real<lower=0> a_bar;        //The mean time in the cafe (Population)
    real<lower=0> sigma;        //The parameter sigma 
}
transformed parameters{
    vector[n_cafes] a;
    a = a_bar + z;
}
model{
    vector[N] mu_i;   
    sigma ~ cauchy(3, 2); 
    a_bar ~ uniform(1, 25);
    //a_bar ~ lognormal( 1.2 , 1);
    z ~ uniform( 0 , 5 );
    for ( i in 1:N ) {
        mu_i[i] = a[cafe[i]];
    }
    y ~ normal(a, sigma);
}





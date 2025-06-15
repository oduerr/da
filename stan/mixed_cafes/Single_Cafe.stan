data{
    int N;
    vector[N] y;
}
parameters{
    real<lower=0> mu;        
    real<lower=0> sigma;
}
model{
    sigma ~ cauchy(3, 2); 
    mu ~ uniform(1, 25);
    y ~ normal(mu, sigma);
}
generated quantities {
 real y_ppd;
 y_ppd = normal_rng(mu, sigma); 
}
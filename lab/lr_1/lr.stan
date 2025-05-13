//Model for linear 
data{
    int<lower=0> N;
    vector[N] y;    
    vector[N] x;
}

parameters{
    real a; 
    real b;
    real<lower=0> sigma;
}

model{
    y ~ normal(a * x + b, sigma);
    a ~ uniform(-100,100);
    b ~ uniform(-100,100);
    sigma ~ uniform(0, 100); 
}


 
// Modeling the waiting times in cafes
// Hierarchical Modelling (non-centered version)

data {
    int N;                       // Total number of visits to cafes
    int n_cafes;                 // Number of different cafes in Konstanz
    vector[N] y;                 // Waiting times for the visits
    array[N] int<lower=1> cafe;  // Cafe index per visit
}

parameters {
    real mu_bar;                 
    real<lower=0> tau;           
    vector[n_cafes] z;           
    real<lower=0> sigma;         
}

model {
    vector[N] mu_i;

    sigma ~ cauchy(3, 2);
    mu_bar ~ normal(10, 5);       
    tau ~ normal(0, 5);           
    z ~ normal(0, 1);             

    for (i in 1:N) {
        mu_i[i] = mu_bar + tau * z[cafe[i]];  // Non-centered mean for each observation
    }

    y ~ normal(mu_i, sigma);
}

generated quantities {
    vector[n_cafes] mu;
    for (j in 1:n_cafes) {
        mu[j] = mu_bar + tau * z[j];          // Compute group means
    }
}
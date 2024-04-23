// Model for linear regression

// Here we define the input and output data for the model
data {
    int<lower=0> N;
    vector[N] y;
    vector[N] x;
}

// Here we define the parameters of the model.
// In our case, we have the slope (a), the intercept (b)
// and the standard deviation of the noise (sigma)
parameters {
    real a;
    real b;
    real<lower=0> sigma;
}

// Here we define the model itself.
// Explicitly using log-likelihood to connect input and output data with the parameters
model {
    // Adding the likelihood of the data given the parameters
    for (i in 1:N) {
        target += normal_lpdf(y[i] | a * x[i] + b, sigma);
    }
}

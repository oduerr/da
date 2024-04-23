data {
  int<lower=0> N;           // Number of observations
  vector[N] time;           // Time index
  vector[N] CO2;            // CO2 measurements
}

parameters {
  real<lower=0> sigma;      // Standard deviation of residuals
  real alpha;               // Intercept
  real b;                   // Slope of the linear trend
  real<lower=0> a;          // Amplitude growth rate of the sinusoidal component
  real omega;               // Frequency of the sinusoidal component
  real phi;                 // Phase shift of the sinusoidal component
}

model {
  for (i in 1:N) {
    // Linear trend plus periodic signal with growing amplitude over time
    real linear_trend = alpha + b * time[i];
    real periodic_signal = a *sin(omega * time[i] + phi);
    
    // Combined model
    CO2[i] ~ normal(linear_trend + periodic_signal, sigma);
  }
}

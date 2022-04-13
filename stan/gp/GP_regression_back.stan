//https://mc-stan.org/docs/2_19/stan-users-guide/gaussian-process-regression.html
// The "incomming" data
data {
  int<lower=1> N;
  real x[N];
  real y[N]; //the obeservations
}
// Creation of m 
transformed data {
 vector[N] m = rep_vector(0, N);
}
// The random variables
parameters {
  vector[N] f;
  //vector[N] m;
  real etasq;
  real rhosq;
  real s2;
}

// Sampling from m and K
model {
  // Non-Random Variables (no samples taken)
  matrix[N, N] K;
  
  //Gaussian Process defined by the variables etasq and rhosq
  etasq ~ exponential( 2.0);
  rhosq ~ exponential( 0.5 );
  s2 ~ exponential( 0.01 );
  
  //Building the Cov Matrix
  for (i in 1:(N - 1)) {
    K[i, i] = 1 + 0.1;
    for (j in (i + 1):N) {
      K[i, j] = etasq * exp(-rhosq * (x[i] - x[j])^2);
      K[j, i] = K[i, j];
    }
  }
  K[N,N] = 1 + 0.1; //not in the loop
  f ~ multi_normal(m, K);
 
 
  //Other Variables 
  s2 ~ exponential( 1 );
  for (i in 1:N) {
    y[i] ~ normal(f, s2);
  }
}



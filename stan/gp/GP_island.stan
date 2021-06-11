//https://mc-stan.org/docs/2_19/stan-users-guide/gaussian-process-regression.html
// The "incomming" data
data {
  int<lower=1> N;
  int T[N];
  matrix[N, N] Dmat;
  vector[N] P;
}
// Creation of m 
transformed data {
  vector[N] m = rep_vector(0, N);
}
// The random variables
parameters {
  vector[N] f;
  real etasq;
  real rhosq;
  real a;
  real b;
  real g;
}

// Sampling from m and K
model {
  // Non-Random Variables (no samples taken)
  matrix[N, N] K;
  vector[N] lambda;
  
  //Gaussian Process defined by the variables etasq and rhosq
  etasq ~ exponential( 2.0);
  rhosq ~ exponential( 0.5 );
  
  //Building the Cov Matrix
  for (i in 1:(N - 1)) {
    K[i, i] = 1 + 0.1;
    for (j in (i + 1):N) {
      K[i, j] = etasq * exp(-rhosq * Dmat[i,j]);
      K[j, i] = K[i, j];
    }
  }
  K[N, N] = 1 + 0.01;
  f ~ multi_normal(m, K);
 
 
  //Other Variables 
  a ~ exponential( 1 );
  b ~ exponential( 1 );
  g ~ exponential( 1 );
  for (i in 1:N) {
    lambda[i] = (a*P[i]^b/g)*exp(f[i]);
    T[i] ~ poisson(lambda[i]);
  }
}



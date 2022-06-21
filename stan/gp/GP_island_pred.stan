//https://mc-stan.org/docs/2_19/stan-users-guide/gaussian-process-regression.html
// The "incomming" data
data {
  int<lower=1> N;  //Number Islands (total) 
  int<lower=1> N1; //<--- Number of Islands in Training 
  int T[N1];
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
  real<lower=0> etasq;
  real<lower=0> rhosq;
  real<lower=0> a;
  real<lower=0> b;
  real<lower=0> g;
}

// Sampling from m and K
model {
  // Non-Random Variables (no samples taken)
  matrix[N, N] K;
  vector[N] lambda;
  
  //Gaussian Process defined by the variables etasq and rhosq
  etasq ~ exponential( 2.0);
  rhosq ~ exponential( 0.5 );
  f ~ std_normal();
  
  //Building the Cov Matrix
  for (i in 1:(N - 1)) {
    K[i, i] = etasq + 0.01;
    for (j in (i + 1):N) {
      K[i, j] = etasq * exp(-rhosq * square(Dmat[i,j]));
      K[j, i] = K[i, j];
    }
  }
  K[N,N] = etasq + 0.01;
  f ~ multi_normal(m, K);
 
  //Other Variables 
  a ~ exponential( 1 );
  b ~ exponential( 1 );
  g ~ exponential( 1 );
  for (i in 1:N1) { //<-------------------    Number of Datapoints used in training
    lambda[i] = (a*P[i]^b/g)*exp(f[i]);
    T[i] ~ poisson(lambda[i]);
  }
}

generated quantities{
  vector[N1] log_lik;
  real TL;
  TL = poisson_rng((a*P[11]^b/g)*exp(f[11])); 
  
  for (n in 1:N1){
    log_lik[n] = poisson_lpmf(T[n] | (a*P[n]^b/g)*exp(f[n]));
  }
}



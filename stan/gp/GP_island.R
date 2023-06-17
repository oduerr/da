library(rstan)
library(magrittr)
library(dplyr)
parallel::detectCores()
options(mc.cores = parallel::detectCores())

###### The Island Example ######
library(rethinking)
data(Kline2)
d <- Kline2

data(islandsDistMatrix)
# display (measured in thousands of km)
Dmat <- islandsDistMatrix
colnames(Dmat) <- c("Ml","Ti","SC","Ya","Fi","Tr","Ch","Mn","To","Ha")
round(Dmat,1)

dat=list(
  N=dim(Dmat)[1], 
  Dmat = Dmat,
  P = d$population,
  T = d$total_tools
)

if (FALSE){
  m1 = stan_model(file='stan/gp/GP_island.stan')
  s = Sys.time()
  fit_island = sampling(m1, dat)
  Sys.time() - s
}
library(cmdstanr)
m1.cmd = cmdstan_model('stan/gp/GP_island.stan')
fit_island = m1.cmd$sample(dat)
options(cmdstanr_max_rows=40)
fit_island
library(tidybayes)
post = spread_draws(fit_island, c(etasq,rhosq,g,b))
# plot 50 functions sampled from posterior
### The decay of the Covariance function
plot(NULL, xlim=c(0,10), ylim=c(0,1), ylab='Covariance', xlab='Distance [1000 km]', main='Posterior Covariance Matrix')
for ( i in 1:50 )
  curve( post$etasq[i]*exp(-post$rhosq[i]*x^2) , add=TRUE , col=col.alpha("black",0.3) )
post %>% ggplot(aes(x=b)) + geom_density() + labs(title='Scaling with population')


loo::loo(fit_island)
##### Make predictions for a new island #####
Lu = runif(10, 1,4) #Lummerland
Dmat_ext = rbind(cbind(Dmat, Lu),c(lu,0))
row.names(Dmat_ext) = c(row.names(Dmat),'Lummer Land')

dat1=list(
  N=dim(Dmat_ext)[1], 
  Dmat = Dmat_ext,
  N1 = 10L,
  P = c(d$population, 3000), #We assume 3000 inhabitans (Lummerland has less)
  T = d$total_tools #We only have 10 entries for the tool
)
m1_pred = stan_model(file='stan/gp/GP_island_pred.stan')
fit_island_pred = sampling(m1_pred, dat1, iter = 6000)

if (FALSE){
  library(cmdstanr)
  m1.cmd = cmdstan_model('GP_island_pred.stan')
  s = Sys.time()
  fit_island = m1.cmd$sample(dat1, chains = 4, adapt_delta = 0.95)
  Sys.time() - s
}
    
fit_island
fit_island_pred
spread_draws(fit_island_pred, TL) %>% ggplot(aes(x=TL)) + geom_density()
loo::loo(fit_island_pred) #p_loo         6.5 1.1


#Alternative Version https://vincentarelbundock.github.io/rethinking2/14.html
if (FALSE) {
library(magrittr)
library(dplyr)
stan_data <- Kline2 %>%
  mutate(society = 1:10) %>%
  compose_data(Dmat = islandsDistMatrix)
stan_program <- "
// cov_GPL2 macro extracted from ulam object with get_stancode
functions{
    matrix cov_GPL2(matrix x, real sq_alpha, real sq_rho, real delta) {
        int N = dims(x)[1];
        matrix[N, N] K;
        for (i in 1:(N-1)) {
          K[i, i] = sq_alpha + delta;
          for (j in (i + 1):N) {
            K[i, j] = sq_alpha * exp(-sq_rho * square(x[i,j]) );
            K[j, i] = K[i, j];
          }
        }
        K[N, N] = sq_alpha + delta;
        return K;
    }
}
data {
    int n;
    int total_tools[n];
    int population[n];
    int society[n];
    matrix[n, n] Dmat;
}
parameters {
    vector[n] k;
    real<lower=0> a;
    real<lower=0> b;
    real<lower=0> g;
    real<lower=0> etasq;
    real<lower=0> rhosq;
}
model{
    vector[n] lambda;
    matrix[n, n] SIGMA;
    rhosq ~ exponential( 0.5 );
    etasq ~ exponential( 2 );
    a ~ exponential( 1 );
    b ~ exponential( 1 );
    g ~ exponential( 1 );
    SIGMA = cov_GPL2(Dmat, etasq, rhosq, 0.01);
    k ~ multi_normal( rep_vector(0,n) , SIGMA );
    for ( i in 1:n ) {
        lambda[i] = (a * population[i]^b/g) * exp(k[society[i]]);
    }
    total_tools ~ poisson( lambda );
}
"
m14.8 <- stan(model_code = stan_program, data = stan_data)
m14.8
}

library(bayesplot)
nuts <- nuts_params(fit_island_pred)
bayesplot::mcmc_scatter(
  fit_island_pred, 
  np = nuts, 
  pars = c("f[11]","f[2]")
)

if (FALSE){
  library(cmdstanr)
  m1.cmd = cmdstan_model('GP_island.stan')
  s = Sys.time()
  fit_island = m1.cmd$sample(dat, chains = 4)#, adapt_delta = 0.95)
  Sys.time() - s
}

if (FALSE){
  m1.cmd_cho = cmdstan_model('attick/GP_island_cholesky.stan')
  s = Sys.time()
  fit_island = m1.cmd_cho$sample(dat, chains = 4)
  Sys.time() - s #About the same
}



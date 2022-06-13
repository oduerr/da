library(rstan)
library(cmdstanr)
library(tidybayes)
library(dplyr)
library(ggplot2)
library(magrittr)


funnel = 
"
data {
  real w_par; //To control the width of the Funnel w/o recompiling just a paramter
  real y; //A single data point
}
parameters {
  real v;  //The spread
  real mu; //The mean value
}
model {
  v ~ normal(0,w_par);
  mu ~ normal(0, exp(v));
  y ~ normal(mu, 1);
}
"
funnel_ncp = 
"
data {
  real w_par; //To control the width of the Funnel w/o recompiling just a paramter
  real y;     //A single data point
}
parameters {
  real v; //The spread
  real mu_tilde; //The N(0,1) variable of the mean value
}
transformed parameters{
  real mu = exp(v) * mu_tilde;
}
model {
  v ~ normal(0, w_par);
  mu_tilde ~ normal(0, 1);
  y ~ normal(mu, 1);
}
"

####### Non-Central Parameterization #####
#mod_funnel_ncp <- stan_model(model_code = funnel_ncp)
#samp_ncp = rstan::sampling(mod_funnel_ncp, list(y=c(0), w_par=5.0), iter=1e5, control = list(adapt_delta = 0.99))
#In my version there are some problems in stan when adapting adapt_delta
mod_funnel_ncp <- cmdstan_model('funnel_ncp.stan')
samp_ncp = mod_funnel_ncp$sample(data=list(y=0, w_par=5.0), adapt_delta = 0.99)
d = spread_draws(samp_ncp, c(v,mu, mu_tilde))  
ggplot(d, aes(x=mu_tilde, y=v)) +  geom_density_2d() +
  geom_point(data = d[1:1000,], aes(x=mu_tilde, y=v), size=0.1) + 
  ggthemes::theme_clean(base_size = 30)

####### Central Parameterization #####
#mod_funnel <- stan_model(model_code = funnel)
#samp_cp = rstan::sampling(mod_funnel, list(y=c(0), w_par=5.0), iter=1e5, control = list(adapt_delta = 0.80), verbose=TRUE)
#traceplot(samp_cp)
mod_funnel <- cmdstan_model('funnel_cp.stan')
samp_cp = mod_funnel$sample(data=list(y=0, w_par=5.0), adapt_delta = 0.99, iter_sampling = 1e4)

samp_cp
d = spread_draws(samp_cp, c(v,mu))  
cor(d$v, d$mu)
ggplot(d, aes(x=mu, y=v)) +  geom_density_2d() +
  geom_point(data = d[1:1000,], aes(x=mu, y=v), size=0.1) + 
  #labs(title = 'w_par = 5.0:: 45341 from 1e5 divergent transitions') + 
  ggthemes::theme_clean(base_size = 30)

####### ReParameterization #####
x = rnorm(1e7, 0.45, sd=1.42)
x = 0.45 + 1.42*rnorm(1e7,0,1)
mean(x)#0.45
sd(x)#1.4206




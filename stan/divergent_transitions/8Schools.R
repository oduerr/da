schools_data = list(
  J = as.integer(8),
  y = c(28,  8, -3,  7, -1,  1, 18, 12),
  sigma = c(15, 10, 16, 11,  9, 11, 10, 18)
)


stan_code_8_schools_cp = 
"
data {
  int<lower=0> J; // number of schools
  vector[J] y; // estimated treatment effect (school j)
  vector<lower=0> [J] sigma; // std err of effect estimate (school j)
}
parameters {
  vector[J]  theta;
  real<lower=0> tau;
  real mu;
}
model {
  mu ~ normal(0,5);
  tau ~ cauchy(0,5);
  theta ~ normal(mu, tau);
  y ~ normal(theta, sigma);
}
"

stan_code_8_schools_ncp = 
  "
data {
  int<lower=0> J; // number of schools
  vector[J] y; // estimated treatment effect (school j)
  vector<lower=0> [J] sigma; // std err of effect estimate (school j)
}
parameters {
  vector[J]  theta_tilde;
  real<lower=0> tau;
  real mu;
}
transformed parameters{
  vector[J] theta;
  for (j in 1:J) 
    theta[j] = mu + tau * theta_tilde[j];
}
model {
  mu ~ normal(0,5);
  tau ~ cauchy(0,5);
  theta_tilde ~ normal(0, 1);
  y ~ normal(theta, sigma);
}
"


library(rstan)
library(tidybayes)
library(dplyr)
library(ggplot2)
library(magrittr)

options(mc.cores = parallel::detectCores())

####### Central Parameterization #####
mod_cp <- stan_model(model_code = stan_code_8_schools_cp)
samp_cp = rstan::sampling(mod_cp, schools_data)
traceplot(samp_cp)

spread_draws(samp_cp, c(mu,tau)) %>% 
  ggplot(aes(x=mu, y=tau)) +  geom_point(size=0.1) + geom_density_2d() 

#######  Non-Central Parameterization ####### 
mod_ncp <- stan_model(model_code = stan_code_8_schools_ncp)
samp_ncp = rstan::sampling(mod_ncp, schools_data)
traceplot(samp_ncp)
samp_ncp
spread_draws(samp_ncp, c(mu,tau)) %>% 
  ggplot(aes(x=mu, y=tau)) +  geom_point(size=0.1) + geom_density_2d() 

dncp = spread_draws(samp_ncp, theta[j])  %>% mutate_at(1, as.factor) 
dncp$para = 'NCP'  

dcp = spread_draws(samp_cp, theta[j]) %>% mutate_at(1, as.factor) 
dcp$para = 'CP'

bind_rows(dncp, dcp) %>% 
ggplot(aes(x=j, y=theta, col=para)) + 
  geom_violin() +  
  geom_boxplot() + labs(title = '8 Schools comparison of different parameterizations')


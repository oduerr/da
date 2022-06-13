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
  vector[J]  theta_tilde; //The N(0,1) variable of theta
  real<lower=0> tau;
  real mu;
}
transformed parameters{
  vector[J] theta = mu + tau * theta_tilde;
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

####### Central Parameterization (the bad one) #####
mod_cp <- stan_model(model_code = stan_code_8_schools_cp)
samp_cp = rstan::sampling(mod_cp, schools_data)

spread_draws(samp_cp, c(mu,tau)) %>% 
  ggplot(aes(x=mu, y=tau)) +  
  geom_point(size=0.1) + 
  geom_density_2d() + 
  ggthemes::theme_clean(base_size = 30)

spread_draws(samp_cp, theta[j]) %>% 
  mutate_at(1, as.factor) %>% #j (in the first column needs to be transformed into factor 
  ggplot(aes(x=j, y=theta)) + 
  geom_violin(fill='lightblue') +  
  labs(title='8 Schools Dataset', 
       subtitle = 'In the centered parametrization') +
  ylab(latex2exp::TeX(r'($\theta$)')) +
  ggthemes::theme_clean(base_size = 20)

########## Diagnosis #######
rstan::check_divergences(samp_cp) #45 of 4000 
traceplot(samp_cp) #does not look so bad
library(bayesplot)
bayesplot::mcmc_neff(bayesplot::neff_ratio(samp_cp))
bayesplot::neff_ratio(samp_cp) #approx 1/4 not so good
bayesplot::neff_ratio(samp_ncp) #most > 0.5

np_cp <- nuts_params(samp_cp)
mcmc_parcoord(samp_cp, np_cp)
mcmc_pairs(samp_cp, np = np_cp, pars = c("mu","tau","theta[1]"))
mcmc_scatter(samp_cp, np = np_cp, pars = c("theta[1]","tau"))
bayesplot::rhat(samp_cp)
mcmc_rhat(bayesplot::rhat(samp_cp))
#######  Non-Central Parameterization ####### 
mod_ncp <- stan_model(model_code = stan_code_8_schools_ncp)
#samp_ncp = rstan::sampling(mod_ncp, schools_data, control = list(adapt_delta = 0.95))
samp_ncp = rstan::sampling(mod_ncp, schools_data)
traceplot(samp_ncp)
samp_ncp
spread_draws(samp_ncp, c(mu,tau)) %>% 
  ggplot(aes(x=mu, y=tau)) +  
  geom_point(size=0.1) + 
  geom_density_2d() 

dncp = spread_draws(samp_ncp, theta[j])  %>% mutate_at(1, as.factor) 
dncp$para = 'NCP'  

dcp = spread_draws(samp_cp, theta[j]) %>% mutate_at(1, as.factor) 
dcp$para = 'CP'

bind_rows(dncp, dcp) %>% 
ggplot(aes(x=j, y=theta, col=para)) + 
  geom_violin() +  
  geom_boxplot() + labs(title = '8 Schools comparison of different parameterizations')

np_ncp <- nuts_params(samp_ncp)
mcmc_parcoord(samp_cp, np_cp)
mcmc_pairs(samp_cp, np = np_cp, pars = c("mu","tau","theta[1]"))

#Comparison
mcmc_scatter(samp_cp, np = np_cp, transform = list(tau = "log"),pars = c("theta[1]","tau"))
mcmc_scatter(samp_ncp, np = np_ncp, transform = list(tau = "log"),pars = c("theta[1]","tau"))


bayesplot::neff_ratio(samp_ncp)

#See also https://mlisi.xyz/post/bayesian-multilevel-models-r-stan/

library(cmdstanr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tidybayes)

setwd("~/Documents/workspace/da/stan/sleep_deprivation/")
set.seed(1) #set seed 

library(lme4) #for data
str(sleepstudy)

ggplot(sleepstudy, aes(y=Reaction, x=Days, col=Subject)) + 
  geom_point() + 
  geom_smooth(method='lm', formula= y~x) + facet_wrap(~Subject)


#### Data For Stan #####
d_stan <- list(Subject = as.numeric(factor(sleepstudy$Subject,labels = 1:length(unique(sleepstudy$Subject)))), Days = sleepstudy$Days, 
               RT = sleepstudy$Reaction/1000, 
               N = nrow(sleepstudy), 
               J = length(unique(sleepstudy$Subject)))
str(d_stan)

##### The simple non-correlated model #### 
options(mc.cores = parallel::detectCores())  # indicate stan to use multiple cores if available
m_s_no_corr <- cmdstan_model('sleep_deprivation.stan') 
#Sampling from the chain
s_s_no_corr = m_s_no_corr$sample(data = d_stan) 
s_s_no_corr$diagnostic_summary()
s_s_no_corr$draws(variables = 'log_lik')
s_s_no_corr %>% spread_draws(beta[i]) %>%  
  ggplot(aes(x=beta, col=as.factor(i))) +
  geom_density()

d = s_s_no_corr %>% 
  spread_draws(log_lik[i]) %>% 
  select(log_lik) 
dd = matrix(d$log_lik, ncol=180) 
library(loo)
loo::waic(dd)#waic       -761.9 46.6
             #elpd_waic   381.0 23.3
loo::loo(dd) #elpd_loo    379.6 23.2


m_s_corr <- cmdstan_model('sleep_deprivation_correlated.stan') 
#Only works with file
 
#Sampling from the chain
s_s_corr = m_s_corr$sample(data = d_stan) 
s_s_corr$diagnostic_summary()
s_s_corr$print()

s_s_corr %>% spread_draws(beta[i]) %>%  
  ggplot(aes(x=beta, col=as.ordered(i))) +
  geom_density()


d = s_s_corr %>% 
  spread_draws(log_lik[i]) %>% 
  select(log_lik) 
dd = matrix(d$log_lik, ncol=180) 
library(loo)
loo::waic(dd) #waic     -768.5 44.0
              #elpd_waic 384.2 22.0
loo::loo(dd)  #elpd_loo  382.6 22.4

s_s_corr %>% spread_draws(Omega[i,j]) %>%
  select(i,j,Omega) %>% 
  summarise(mean(Omega)) #0.0684 small correlation




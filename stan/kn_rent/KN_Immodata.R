library(readr)
########### Reading the KN County Data #########
if (FALSE){
  setwd("~/Documents/workspace/da/stan/kn_rent/")
  #Data downloaded from https://www.kaggle.com/datasets/corrieaar/apartment-rental-offers-in-germany?select=immo_data.csv
  immo_data <- read_csv("~/Downloads/immo_data.csv")
  dim(immo_data) #268850     49
  if (FALSE) { #Only KN
    cities = c('Konstanz')
    KN_City = immo_data %>% 
      filter(immo_data$regio3 %in% cities)
    nrow(KN_City) #85
  }
  KN_Kreis = immo_data %>% 
    filter(regio2 == 'Konstanz_Kreis') %>% 
    filter(baseRent > 0) 
  ###### Add a training data flag ######
  (N = nrow(KN_Kreis))
  set.seed(1) #Need to check that all in testing are in training
  KN_Kreis$training=sample(c(TRUE, FALSE), size = N, prob=c(0.8,0.2), replace = TRUE)
  KN_Kreis %>% 
    filter(training) %>% count(regio3) 
  saveRDS(KN_Kreis, 'KN_Kreis.rds')
} else{
  KN_Kreis = readRDS('KN_Kreis.rds')
}

sum(is.na(KN_Kreis$totalRent))#51
sum(is.na(KN_Kreis$baseRent))#0
(N = nrow(KN_Kreis))

####### No pooling ####
p_base = KN_Kreis %>% 
  filter(training) %>% 
  ggplot(aes(x=livingSpace, y=baseRent)) +
  geom_point() + 
  geom_smooth(method='lm', formula= y~x) + 
  labs(subtitle = 'Rental Prices in Konstanz County (Training Data)') 
p_base + ggthemes::theme_hc(base_size = 12) 
ggsave('KN_all.pdf', width = 14, height = 14, units = 'cm')


####### All completely pooled #####
p_base + facet_wrap(~regio3) + 
  coord_cartesian(xlim = c(0, 250), ylim = c(0, 3000)) + 
  ggthemes::theme_hc(base_size = 6) 
ggsave('KN_facet.pdf', width = 14, height = 14, units = 'cm')

p_base = KN_Kreis %>% 
  filter(training) %>% 
  ggplot(aes(x=livingSpace, y=baseRent, col=regio3)) +
  geom_point() + 
  geom_smooth(method='lm', formula= y~x, se=FALSE) + 
  labs(subtitle = 'Rental Prices in Konstanz County (Training Data)') 
p_base + ggthemes::theme_hc(base_size = 12) 
ggsave('KN_all_color.pdf', width = 14, height = 14, units = 'cm')

###### Create the data for STAN ####
# Here we add a city identifier
cities = sort(unique(KN_Kreis$regio3))
J = length(cities)
cities_numbers = data.frame(regio3 = cities, id = 1:J)
KN_Kreis = KN_Kreis %>% right_join(cities_numbers, by='regio3')
KN_train = KN_Kreis %>% filter(training)
KN_test = KN_Kreis %>% filter(!training)

#Possible scaling
#Hack Attack for no scaling
x_mean = mean(KN_train$livingSpace)
x_sd = sd(KN_train$livingSpace)
y_mean = mean(KN_train$baseRent)
y_sd = sd(KN_train$baseRent) 

kn_stan_dat = list(
  N = nrow(KN_train),
  y = (KN_train$baseRent - y_mean)/y_sd,
  x = (KN_train$livingSpace - x_mean)/x_sd,
  y = KN_train$baseRent,
  x = KN_train$livingSpace,
  j = KN_train$id,
  
  N_t = nrow(KN_test),
  y_t = (KN_test$baseRent - y_mean)/y_sd,
  x_t = (KN_test$livingSpace - x_mean)/x_sd,
  y_t = KN_test$baseRent,
  x_t = KN_test$livingSpace,
  j_t = KN_test$id,
  J = J,
  
  y_t_unscaled = KN_test$baseRent
)
kn_stan_dat

########## Using lmre ###########
library(lme4)
d = data.frame(
  x = kn_stan_dat$x , 
  y = kn_stan_dat$y ,
  j = as.factor(kn_stan_dat$j)
)

d_t = data.frame(
  x = kn_stan_dat$x_t , 
  y = kn_stan_dat$y_t ,
  j = as.factor(kn_stan_dat$j_t)
)

pred = function(mod, d_t, scale=FALSE) {
  p = predict(mod, d_t)
  if (scale){
    p = p*y_sd + y_mean 
    return (sqrt(mean((p - kn_stan_dat$y_t_unscaled)^2)))
  } 
  return (sqrt(mean((p - kn_stan_dat$y_t)^2)))
}
pred(lm(y ~ x, d), d_t, scale = TRUE)
pred(lm(y ~ 0 + x, d), d_t, scale = TRUE)
pred(lm(y ~ x + j, d_t), scale = TRUE)
pred(lm(y ~ 0 + x + j, d_t), scale = TRUE)
pred(lmer(y ~  x + (x | j), d), d_t, scale = TRUE) #Not working when unscaled

########## Using  brms ###########
library(brms)
get_test_rmse = function(mod, scaled=FALSE) {
  if(scaled){
    p = predict(mod, d_t)[,1]
    diff = kn_stan_dat$y_t_unscaled - (p*x_sd + x_mean)
  } else {
    diff = d_t$y - predict(mod, d_t)[,1]
  }
  sqrt(mean(diff^2))
}


###### model with varying slope but fixed intercept #####
b0 <- brm(data = d, family = gaussian, 
          y ~ 1 + x + (0 + x | j),
          prior = c(
            prior(normal(0, 1),  class = Intercept), #Base Euro / qm
            prior(normal(0, 1), class = b),           #Euro / qm
            prior(exponential(1.0), class = sd),
            prior(exponential(1.0), class = sigma)
          ),              
          #iter = 8000, warmup = 5000, chains = 4, 
          sample_prior = TRUE,
          cores = 8
)
stanplot(b0, type = "trace") #No divergences to plot, chains look good
np <- nuts_params(b0) 
sum(subset(np, Parameter == "divergent__")$Value) #0
range(rhat(b0)) #0.999780 1.001286
mcmc_plot(b0, type = "rhat")
mcmc_plot(b0, type = "neff_hist")
np_cp <- nuts_params(b0)
bayesplot::mcmc_parcoord(b0)

get_test_rmse(b0) #~227(unscaled) 
posterior_summary(b0)

b1 <- brm(data = d, family = student, 
          y ~ 1 + x + (0 + x | j),
          prior = c(
            prior(normal(0, 100),  class = Intercept), #Base Euro / qm
            prior(normal(5, 10), class = b),           #Euro / qm
            prior(exponential(1.0e-2), class = sd),
            prior(exponential(1.0e-2), class = sigma)
          ),              
          iter = 8000, warmup = 5000, chains = 4, 
          sample_prior = TRUE,
          cores = 8
)
get_test_rmse(b1) #~251
loo::loo(b1)
posterior_summary(b1) %>% round(digits = 2) #nu approx 3


b3 <- brm(data = d, family = gaussian, 
    y ~ 1 + x + (1 + x | j),
    prior = c(
      prior(normal(0, 100),  class = Intercept), #Base Euro / qm
      prior(normal(5, 10), class = b),           #Euro / qm
      prior(exponential(1.0e-2), class = sd),
      prior(exponential(1.0e-2), class = sigma),
      prior(lkj(1), class = cor)),               #Very weak prior
    iter = 8000, warmup = 5000, chains = 4, 
    sample_prior = TRUE,
    cores = 8
) # There were 59 divergent transitions after warmup.
get_test_rmse(b3) #~225.7816

b4 <- brm(data = d, family = student, 
          y ~ 1 + x + (1 + x | j),
          prior = c(
            prior(normal(0, 100),  class = Intercept), #Base Euro / qm
            prior(normal(5, 10), class = b),           #Euro / qm
            prior(exponential(1.0e-2), class = sd),
            prior(exponential(1.0e-2), class = sigma),
            prior(lkj(1), class = cor)),               #Very weak prior
          iter = 8000, warmup = 5000, chains = 4, 
          sample_prior = TRUE,
          cores = 8
)
get_test_rmse(b4) #~248.2817
posterior_summary(b4) %>% round(digits = 2)

library(tidybayes)
spread_draws(res, b_Intercept)$b_Intercept %>% hist(1000, main='global Intercept') 
spread_draws(res_simple, r_j[id, term]) %>% 
  filter(term == 'x') %>% 
  right_join(cities_numbers, by = 'id') %>% 
  ggplot(aes(x=r_j, y=regio3)) + 
  geom_density_ridges() +
  labs(title = title, x = 'Slope Euro/sqm') +
  theme_ridges()

spread_draws(res, r_j[id, term]) %>% 
  filter(term == 'Intercept') %>% 
  right_join(cities_numbers, by = 'id') %>% 
  ggplot(aes(x=r_j, y=regio3)) + 
  geom_density_ridges() +
  labs(title = title, x = 'Intercept Euro') +
  coord_cartesian(xlim = c(-50, 50)) + 
  theme_ridges()


########## Stan Fitting ###########
file = 'kn_hier.stan'
title = 'Konstanz County: Hierachical Model Gauss'
title_short = 'KN_h'
options(mc.cores = parallel::detectCores())
library(rstan)
rm(kn_m);rm(kn_s)
kn_m = stan_model(file = file)
kn_s = sampling(kn_m, data=kn_stan_dat) 
kn_s
#Error in unserialize(socklist[[n]]) : error reading from connection
#This seems to happen, when options like max_treedepth are set
if (FALSE){
  library(cmdstanr)
  kn_mc =  cmdstan_model('kn_hier.stan')
  kn_sc = kn_mc$sample(data = kn_stan_dat)
}
###Diagnostics of the fit
stanplot(b0, type = "trace")
traceplot(kn_s) #Slow Mixing


print(kn_s)
loo::loo(kn_s)
library(tidybayes)
library(bayesplot)
library(ggridges)

spread_draws(kn_s, u[i,j]) %>%
  filter(i == 2) %>% 
  right_join(cities_numbers, by = c("j" = "id")) %>% 
  ggplot(aes(x=u, y=regio3)) + 
  geom_density_ridges() +
  labs(title = title, x = 'Slope Euro/sqm') +
  theme_ridges()
ggsave(paste0(title_short, '_slope_stan.pdf'), width = 28, height = 28, units = 'cm')

spread_draws(kn_s, beta[i]) %>%
  ggplot(aes(x=beta, col=as.factor(i))) + geom_density() + 
  labs(title = 'Mean of Intercept[1] and slope[2]', x = 'Slope Euro/sqm') 

spread_draws(kn_s, pu[i]) %>%
  ggplot(aes(x=pu, col=as.factor(i))) + geom_density() + 
  labs(title = 'Spread of Intercept[1] and slope[2]', x = 'Slope Euro/sqm') 

spread_draws(kn_s, sigma_e)$sigma_e %>% hist(100)


kn_stan_dat$N_t
spread_draws(kn_s, y_t_pred[i,j])
d = rstan::extract(kn_s)
preds = data.frame(
  #pred_mu = y_mean + y_sd * colMeans(as.matrix(d$y_t_pred, ncol=kn_stan_dat$N_t)),
  pred_mu = colMeans(as.matrix(d$y_t_pred, ncol=kn_stan_dat$N_t)),
  y_obs = kn_stan_dat$y_t,
  city = as.factor(kn_stan_dat$j_t)) 
preds %>% ggplot(aes(x=pred_mu, y=y_obs, col=city)) + geom_point()
sqrt(mean((preds$pred_mu - preds$y_obs)^2))

library(rstan)




  
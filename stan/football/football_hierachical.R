library(tidyverse)
data <- read.csv('https://github.com/oduerr/da/raw/master/stan/football/bundesliga_2025.csv', stringsAsFactors = FALSE)
head(data)

ng = nrow(data)
teams = data$Home %>%
  unique() %>%
  sort()
nt = length(teams)
ht = data$ht
at = data$at

my_data = list(
  nt = nt, 
  ng = ng,
  ht = data$ht,
  at = data$at,
  s1 = data$HomeGoals,
  s2 = data$AwayGoals
)

library(cmdstanr)
options(mc.cores = parallel::detectCores())
library(here)
hmodel = cmdstan_model(here("stan", "football", "hier_model_cor_nocholesky.stan"))
hfit = hmodel$sample(data = my_data)



homes =  hfit %>% tidybayes::spread_draws(home) %>% select('home')
hist(homes$home, 50)
abline(v=0)

#setwd("~/Documents/workspace/da/stan/howell1")
library(cmdstanr)
library(rethinking)
data(Howell1)
d = Howell1[Howell1$age >= 18, ]

#res = stan('unconditional.stan', data = list(N=nrow(d), height=d$height))
model = cmdstan_model('unconditional.stan') #Compile
samples_cmd = model$sample(data = list(N=nrow(d), height=d$height))
d = samples_cmd$draws(format = "draws_matrix") 




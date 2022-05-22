#setwd("~/Documents/workspace/da/stan/howell1")
#https://vincentarelbundock.github.io/rethinking2/05.html
library(rstan)
library(rethinking) #For the data set
library(tidybayes)
library(ggplot2)
data(Howell1)
d = Howell1[Howell1$age >= 18, ]

data = list(
        sex = as.integer(d$male + 1), #Adding one to be used as index variable
        N = nrow(d), 
        W = d$weight,
        H = d$height - mean(d$height) #Scaled
)

model_weight_sex <- stan(file = 'categorical_weight_sex.stan', data = data, iter = 8000) #Need quite high number of iteration
traceplot(model_weight_sex)
model_weight_sex
stan_dens(model_weight_sex, 'a')
post = rstan::extract(model_weight_sex)
post_a = spread_draws(model_weight_sex, a[sex])  
post_a$sex = as.factor(post_a$sex)
ggplot(post_a) + geom_density(aes(x=a, color=sex), size = 1) + ggthemes::theme_pander()

post_a$sim_weight = post$a[,1] 


ggplot(data.frame(post$a)) + geom_density(aes(x=X1))

plot(density(post$a[,1]), xlim=c(40, 50), main='post densities for a')
lines(density(post$a[,2]))


model <- stan(file = 'categorical.stan', data = data, iter = 8000) #Need quite high number of iteration
traceplot(model)
model
stan_dens(model, 'diff_fm')

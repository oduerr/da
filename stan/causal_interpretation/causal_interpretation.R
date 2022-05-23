#setwd("~/Documents/workspace/da/stan/howell1")
library(rstan)
library(rethinking) #For the data set
library(tidybayes)
library(ggplot2)
data(Howell1)
d = Howell1[Howell1$age >= 18, ]
### Problem with standard coding
lm(weight ~ male, d)
#lm uses the following codeing
model.matrix(~ male,d) 

data = list(
        sex = as.integer(d$male + 1), #Adding one to be used as index variable
        N = nrow(d), 
        W = d$weight,
        H = d$height - mean(d$height) #Scaled
)

##### Weight ~ Sex #######
model_weight_sex <- stan(file = 'categorical_weight_sex.stan', data = data, iter = 8000) #Need quite high number of iteration
traceplot(model_weight_sex)
model_weight_sex
stan_dens(model_weight_sex, 'a')
post = rstan::extract(model_weight_sex)
lm(W ~ sex, data=data)
mean(post$a[,2]) - mean(post$a[,1])
mean(post$a[,2] - post$a[,1])#6.765878, 6.776768

#Posterior of parameter a
post_a = spread_draws(model_weight_sex, a[sex])  
post_a$sex = as.factor(post_a$sex)
ggplot(post_a) + geom_density(aes(x=a, color=sex), size = 1) + ggthemes::theme_pander()

#Posterior predictive
NSim = nrow(post$a)
sim_weights = data.frame(
  W = c(rnorm(NSim, post$a[,1], post$sigma),
        rnorm(NSim, post$a[,2], post$sigma)),
  sex = c(rep('FEMALE', NSim), rep('MALE', NSim)))
ggplot(sim_weights) + geom_density(aes(x=W, color=sex), size = 1) + ggthemes::theme_pander()

##### Full Bayes #######
model_full <- stan(file = 'full_bayes.stan', data = data, iter = 8000) #Need quite high number of iteration
model_full
post = rstan::extract(model_full)
N = nrow(post$a)
#Do S = 1
H_S1 = rnorm(N, post$a_H[,1], post$sigma_H)
W_S1 = rnorm(N, post$a[,1] + post$b[,1]*H_S1, post$sigma) 

#Do S = 1
H_S2 = rnorm(N, post$a_H[,2], post$sigma_H)
W_S2 = rnorm(N, post$a[,2] + post$b[,2]*H_S2, post$sigma) 

mean(W_S2) - mean(W_S1) #6.72563, 6.790385
mean(W_S2 - W_S1)

post_b = spread_draws(model_full, b[sex])  
post_b$sex = as.factor(post_b$sex)
ggplot(post_b) + geom_density(aes(x=b, color=sex), size = 1) + ggthemes::theme_pander()

post_a = spread_draws(model_full, a[sex])  
post_a$sex = as.factor(post_a$sex)
ggplot(post_a) + geom_density(aes(x=a, color=sex), size = 1) + ggthemes::theme_pander()


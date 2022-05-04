#setwd("~/Documents/workspace/da/stan/howell1")
library(cmdstanr)
library(rethinking) #For the data set
data(Howell1)
d = Howell1[Howell1$age >= 18, ]

model = cmdstan_model('conditional.stan') #Compile
samples_cmd = model$sample(data = list(
  N=nrow(d), 
  height=d$height,
  hbar = mean(d$height),
  weight = d$weight
  ))
post_s = samples_cmd$draws(format = "df") 
post_s[1:5,2:4]

#It is a 
plot(post_s$b, post_s$a)
cor(post_s$b, post_s$a)

#Mu from samples
mu_160 = post_s$a + post_s$b * (160 - mean(d$height))
hist(mu_160, freq = FALSE)
lines(density(mu_160))

w_160 = rnorm(length(mu_160), mu_160, post_s$sigma)
hist(w_160, freq = FALSE)
lines(density(mu_160), col='red')
lines(density(w_160))




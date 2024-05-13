#setwd("~/Documents/workspace/da/stan/howell1")
library(cmdstanr)
library(tidybayes)
library(bayesplot)
if (FALSE){
  #devtools::install_github("rmcelreath/rethinking@slim")
  devtools::install_github("rmcelreath/rethinking")
}
library(rethinking) #For the data set
data(Howell1)
d = Howell1[Howell1$age >= 18, ]

#### Prior Predictive ####
##### Loop for didactic purposes ####
N = 1e6
prior_pred = rep(NA, N)
for (n in 1:N){
  prior_mu = rnorm(1, mean = 178, sd = 20) #Average Height of Person
  prior_sigma = runif(1, min = 0, max = 50) #Height Variability
  prior_pred[n] = rnorm(1, mean = prior_mu, sd = prior_sigma)
}
plot(density(prior_pred), main = "Prior Predictive Distribution", xlab = "Height", ylab = "Density")

##### In Short Vectorized Form ####
sample_mu = rnorm(1e4, mean = 178, sd = 20)
sample_sigma = runif(1e4, min = 0, max = 50)
prior_pred = rnorm(1e4, mean = sample_mu, sd = sample_sigma)
plot(density(prior_pred))

#### Posterior Predictive ####
#res = stan('unconditional.stan', data = list(N=nrow(d), height=d$height))
model = cmdstan_model('stan/howell1/unconditional.stan') #Compile
samples_cmd = model$sample(data = list(N=nrow(d), height=d$height), iter_sampling = 1e3)
samples_cmd
post_s = samples_cmd$draws(format = "df") 
post_s[,2:3]

##### Posterior ####
s = samples_cmd$draws() #We need to call draws() 2023
cor(post_s$mu, post_s$sigma)
mcmc_pairs(s, mu, sigma)
#mcmc_trace(s)

plot(density(post_s$mu), main = "Posterior Distribution of mu", xlab = "Mu", ylab = "Density")
plot(density(post_s$sigma), main = "Posterior Distribution of sigma", xlab = "Sigma", ylab = "Density")

#Posterior Predictive
post_pred = rep(NA, nrow(post_s))
for (i in 1:length(post_s$mu)){
  mu_i = post_s$mu[i]
  sigma_i = post_s$sigma[i]
  post_pred[i] = rnorm(1, mean = mu_i, sd=sigma_i) #One Sample
}
plot(density(post_pred), col='red', main = "Posterior Predictive Distribution", xlab = "Height", ylab = "Density")
rug(d$height)
lines(density(post_s$mu), col='blue')

# Vectorized
post_pred = rnorm(nrow(post_s), mean = post_s$mu, sd=post_s$sigma)

# Posterior vs Posterio Predictive
# Plotting the density of mu
plot(density(post_s$mu), main = "Posterior and PPD", xlab = "Mu / Height", ylab = "Density", xlim=c(130,180), ylim=c(0,0.3))
# Overlaying with another density in red
lines(density(post_pred), col='red')
# Adding rug marks for individual data points
rug(d$height)
# Adding a legend, including a representation for the rug
legend("topright",  # Position of the legend in the top right corner of the plot
       legend = c("Posterior of mu", "Posterior Predictive", "Observed"),  # Labels for the lines and the rug
       col = c("black", "red", "black"),  # Colors corresponding to the lines and the rug
       lty = c(1, 1, NA),  # Line types for the lines, NA for the rug as it has no line
       pch = c(NA, NA, 124),  # Plotting characters: NA for lines, and a specific character for the rug
       cex = 0.8)  # Text scaling factor to reduce legend text size


plot(density(post_pred), col='red', main = "Posterior Predictive Distribution", xlab = "Height", ylab = "Density")
lines(density(post_s$pred_height), col='blue')



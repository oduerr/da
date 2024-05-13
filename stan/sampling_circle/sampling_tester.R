library(cmdstanr)

# Compile the model
stan_file <- "stan/sampling_circle/sampling_euclidean.stan"
mod <- cmdstan_model(stan_file)

# Perform sampling
fit <- mod$sample(chains = 4, iter_sampling = 1000)

# Check the results
print(fit)
draws = as.data.frame(fit$draws(format = 'df'))
names(draws) = c('lp__', 'x', 'y')
plot(draws[,3], draws[,2])

# Other diagnostics
#fit$metadata()$

##### Sampling in Polar Coordinates #####
# Compile the model assuming the path to your revised polar coordinates model
stan_file <- "stan/sampling_circle/sampling_polar.stan"
mod <- cmdstan_model(stan_file)

# Perform sampling
fit_polar <- mod$sample(chains = 4, iter_sampling = 1000)

# Check the results
print(fit_polar)
draws = as.data.frame(fit_polar$draws(format = 'df'))
names(draws) = c('lp__', 'r', 'theta')

# Transform back to Cartesian coordinates for plotting
draws$x = draws$r * cos(draws$theta)
draws$y = draws$r * sin(draws$theta)
plot(draws$x, draws$y, main = "Samples in Cartesian Coordinates", xlab = "x", ylab = "y", asp = 1)

# Optional: plot r and theta
par(mfrow=c(1,2))
hist(draws$r, main = "Histogram of Radius (r)", xlab = "r", breaks = 30)
hist(draws$theta, main = "Histogram of Angle (theta)", xlab = "Theta (radians)", breaks = 30)

par(mfrow=c(1,1))


fit
fit_polar


############# Sampling in Arbitrary Dimensions #############
# Here we on a complete sphere

stan_file <- "stan/sampling_circle/sampling_polar_abitrary_dim.stan"
ness_polar = rep(NA, 20)
print(fit_polar)
for (D in 1:20){
  mod <- cmdstan_model(stan_file)
  fit_polar <- mod$sample(chains = 4, iter_sampling = 1000, data = list(D = D))
  ness_polar[D] = fit_polar$summary('r')$ess_bulk
}
ness_polar


stan_file <- "stan/sampling_circle/sampling_euclidean_abitrary_dim.stan"
ness_euclid = rep(NA, 20)
for (D in 1:20){
  mod <- cmdstan_model(stan_file)
  fit <- mod$sample(chains = 4, iter_sampling = 1000, data = list(D = D))
  ness_euclid[D] = fit$summary('radius')$ess_bulk
}
ness_euclid

### Plotting and saving the result
pdf("stan/sampling_circle/sampling_ess.pdf")
plot(1:20, ness_polar, type='l', main = "Effective Sample Size (Sampling Euclidean / Polar Koordinates)", xlab = "Dimensionality", ylab = "ESS")
lines(1:20, ness_euclid, col = 'red')
legend("topright", legend=c("Polar Coordinates", "Euclidean Coordinates"), col=c("black", "red"), lty=1:1, cex=0.8)
dev.off()
### Saving the plot




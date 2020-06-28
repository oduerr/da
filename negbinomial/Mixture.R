N = 100000
lambda = rgamma(N, shape=2,scale = 4) 
res = rpois(N, lambda) #Poissonians with rate according to 

#Plot of the Gamma
mean(lambda)#approx 8
plot(density(lambda))


#Plot of the resulting mixed distribution
plot(table(res)/N, xlim = c(0,40))
mu = mean(res) #approx 8

#Fit of a poissian
xs = 0:42
points(xs, dpois(xs, lambda=(mu)), col='red')
lines(xs, dpois(xs, lambda=(mu)), col='red')


phi=2
mu = mean(res)
v = var(res)
phi = mu^2/(v-mu) #approx 2
#phi = 1e8 same as poisson
points(xs,dnbinom(xs, mu = mu, size=phi), col='green')
lines(xs,dnbinom(xs, mu = mu, size=phi), col='green')
legend('topright', legend = c('Poisson', 'NB'), col=c('red', 'green'), lty=c(1,1))

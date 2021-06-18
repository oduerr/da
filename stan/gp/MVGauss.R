library(MASS)
library(ggplot2)

N = 1e4
Sigma = matrix(c(1,0.9,0.9,1), nrow=2)
Sigma
d_0.9 = data.frame(mvrnorm(n = N, Sigma = Sigma, mu=c(0,0)))
ggplot(d_0.9, aes(x=X1, y=X2)) +  geom_point(size=0.2, alpha=0.4) + geom_density2d()


y = rnorm(1000, 0, 42)
sd(y)

y = rnorm(1e6, 0, 1) * 42
sd(y)

#The RBF Kernel
f = function(d) a^2 * exp(-0.5*d^2/r^2)
r = 1;a = sqrt(2)
curve(f, 0, 10, col='black', xlab='Distance', ylab='Covariance')
r = 1;a = 1
curve(f, 0, 10, col='green',add=TRUE)
r = 0.5;a = 1
curve(f, 0, 10, col='red',add=TRUE)
legend('topright', legend = c('r = 1;a = sqrt(2)', 'r = 1;a = 1', 'r = 0.5;a = 1'),col=c('black', 'green', 'red'), lt=1)

# With Cholesky Decomposition
Sigma = matrix(c(1,0.9,0.9,1), nrow=2)
K_L = chol(Sigma)
K_L
z = matrix(rnorm(1e4, mean=0, sd=1), ncol=2) 
x = z %*% K_L
ggplot(data.frame(x), aes(x=X1, y=X2)) +  geom_point(size=0.2, alpha=0.4) + geom_density2d()





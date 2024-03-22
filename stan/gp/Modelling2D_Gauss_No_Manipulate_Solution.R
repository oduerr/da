library(MASS)
library(ggplot2)
library(manipulate)
N = 1000

mu1=0
mu2=1
s1=1.5
s2=0.5
cor=-0.5

rho = matrix(c(1,cor,cor,1), nrow=2)
sigma = matrix(c(s1,0,0,s2), nrow=2)
K = sigma %*% rho %*% sigma

#Sampling of data from MV-Gaussian
d = data.frame(mvrnorm(n = N, Sigma = K, mu=c(mu1,mu2)))

print(sd(d$X1))
#Drawing of the plot
ggplot(d, aes(x=X1, y=X2)) +
  geom_point(size=0.5, alpha=0.5) +
  stat_ellipse(col='blue', level = 0.3) +
  stat_ellipse(col='blue', level = 0.6) +
  stat_ellipse(col='blue', level = 0.8) 

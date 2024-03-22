library(MASS)
library(ggplot2)
library(manipulate)
N = 1000
manipulate({
  rho = matrix(c(1,cor,cor,1), nrow=2)
  sigma = matrix(c(s1,0,0,s2), nrow=2)
  K = sigma %*% rho %*% sigma
  
  #Sampling of data from MV-Gaussian
  d = data.frame(mvrnorm(n = N, Sigma = K, mu=c(mu1,mu2)))
  cat('\n SD (X1,X1) (')
  cat(round(sd(d$X1), 2))
  cat(', ')
  cat(round(sd(d$X2), 2))
  cat(') COR (X1,X2)')
  cat(round(cor(d$X1, d$X2),2))
  cat('  MEAN (X1,X1) (')
  cat(round(mean(d$X1), 2))
  cat(', ')
  cat(round(mean(d$X2), 2))
  cat(', ')
  #Drawing of the plot
  gg = ggplot(d, aes(x=X1, y=X2)) +
    geom_point(size=0.5, alpha=0.5) +
    stat_ellipse(col='blue', level = 0.3) +
    stat_ellipse(col='blue', level = 0.6) +
    stat_ellipse(col='blue', level = 0.8) +
    xlim(-5,5) + ylim(-5,5) + theme_bw()
  print(gg)
},
s1=slider(0,5,1, step = 0.1),
s2=slider(0,5,1, step = 0.1),
cor=slider(-1,1,0, step = 0.1),
mu1=slider(-5,5,0, step = 0.5),
mu2=slider(-5,5,0, step = 0.5)
)


Sigma = matrix(c(1,0.8,0.8,1), nrow=2)
Sigma
#3 DOF
chol(Sigma)





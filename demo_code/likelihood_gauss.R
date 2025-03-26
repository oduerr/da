ys <- c(-0.033, -0.76, 2.02, 1.13, 0.65, 0.49, 0.76, 0.40, 0.10, 0.61)


#Likelihood for all data points
l <- function(ys, mu, sigma) {
  lik <- 1;
  for (y in ys) {
    lik <- lik * dnorm(y, mean=mu, sd=sigma)
    }
  return (lik)
}

l(ys, 0, 1)

require ("manipulate") 
manipulate (
  {
    yvals <- seq(-5,5,0.05);
    lik <- l(ys, mu, sigma);
    print(lik)
    plot(yvals, dnorm(yvals, mean=mu, sd=sigma),
         type="l",xlab="Daten", ylab="", main=paste0("Likelihood = ",
                                                     lik), xlim=c(-2,2)); points(ys, rep(0,length(ys)),cex=2)
  },
  mu = slider (-5 ,5, initial=0.1, step=0.01), sigma = slider (0,5, initial=0.1, step=0.01)
)


mean(ys)
sd(ys)
l(ys, mean(ys), sd(ys))
### TODO Chance to log-likelihood

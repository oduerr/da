library(cmdstanr)
library(magrittr)
library(dplyr)
library(tidybayes)
parallel::detectCores()
options(mc.cores = parallel::detectCores())

###### The Island Example ######
library(rethinking)
data(Kline2)
d <- Kline2

data(islandsDistMatrix)
# display (measured in thousands of km)
Dmat <- islandsDistMatrix
colnames(Dmat) <- c("Ml","Ti","SC","Ya","Fi","Tr","Ch","Mn","To","Ha")
round(Dmat,1)

dat=list(
  N=dim(Dmat)[1], 
  Dmat = Dmat,
  P = d$population,
  T = d$total_tools
)



m1.cmd = cmdstan_model('stan/gp/GP_island.stan')
fit_island = m1.cmd$sample(dat)
options(cmdstanr_max_rows=40)
fit_island

post = spread_draws(fit_island, c(etasq,rhosq,g,b))
# plot 50 functions sampled from posterior
### The decay of the Covariance function
plot(NULL, xlim=c(0,10), ylim=c(0,1), ylab='Covariance', xlab='Distance [1000 km]', main='Posterior Covariance Matrix')
for ( i in 1:50 )
  curve( post$etasq[i]*exp(-post$rhosq[i]*x^2) , add=TRUE , col=col.alpha("black",0.3) )


####### Below Solution #####

post %>% ggplot(aes(x=b)) + geom_density() + labs(title='Scaling with population')
df = post %>% spread_draws(b) 
mean(df$b)
P=2
(P**mean(df$b)) #1.20 you expect about 20% more tools



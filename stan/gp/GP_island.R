library(rstan)
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
fit_island = stan(file = '~/Documents/workspace/da/stan/gp/GP_island.stan', 
           data=list(
              N=dim(Dmat)[1], 
              Dmat = Dmat,
              P = d$population,
              T = d$total_tools),
           chains = 4,
           iter = 10000)
fit_island
traceplot(fit_island)

post = extract(fit_island)


# plot 50 functions sampled from posterior
### The decay of the Covariance function
plot(post$etasq, post$rhosq)
plot(NULL, xlim=c(0,10), ylim=c(0,2), ylab='Covariance', xlab='Distance [1000 km]')
for ( i in 1:50 )
  curve( post$etasq[i]*exp(-post$rhosq[i]*x^2) , add=TRUE , col=col.alpha("black",0.3) )

# xs = seq(0,10, length.out=100)
# for (i in 1:100) {
#   #etasq * exp(-rhosq * Dmat[i,j]);
#   lines(xs, post$etasq[i] * exp(-post$rhosq[i] * xs^2), col='gray')  
# }

## R code 14.42
# compute posterior median covariance among societies
K <- matrix(0,nrow=10,ncol=10)
for ( i in 1:10 )
  for ( j in 1:10 )
    K[i,j] <- median(post$etasq) * 
      exp( -median(post$rhosq) * islandsDistMatrix[i,j]^2 )
diag(K) <- median(post$etasq) + 0.01
Rho <- round( cov2cor(K) , 2 )
# add row/col names for convenience
colnames(Rho) <- c("Ml","Ti","SC","Ya","Fi","Tr","Ch","Mn","To","Ha")
rownames(Rho) <- colnames(Rho)
Rho


T1 = post$f[,1] * post$a * d$population[1]^post$b/post$g
hist(T1[T1<100], xlim=c(0,13))



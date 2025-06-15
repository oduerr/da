# Inspired by https://raw.githubusercontent.com/rmcelreath/stat_rethinking_2022/main/scripts_animation/12_intro_multilevel_models.r

#library(ellipse)
library(here)
library(tidybayes)
library(tidyverse)
########################################
# coffee golem
# visit cafes, record waiting time, update posterior
library(rstan)
library(cmdstanr)
# sim data first
#################
# Single Cafe
# Use the same data as Mac-Ereath
set.seed(7)
n_cafes <- 1
w <- c(3,5,3,1,3)
n_visits <- 20
cafe <- sample(1:n_cafes,size=n_visits,replace=TRUE)
#w[cafe] = 4
y <- rpois(n_visits,w[cafe])

m1 <- cmdstan_model("stan/mixed_cafes/Single_Cafe.stan", compile = TRUE )
#m1 = rstan::stan_model(model_code=m1_code) #

par(mfrow=c(2,3))
# loop
for (N in 0:5){
  if (N > 0) {
    #Stan code
    #m1_samples = sampling(m1, data = list(N=N, y=y[1:N]))
    #m1_samples
    #extract(m1_samples)
    #traceplot(m1_samples)
    
    #Cmd Stan code
    m1_samples = m1$sample(data = list(N=N, y=y[1:N]))
    post_s = m1_samples$draws(format = "df") 
    post_pred = post_s$y_ppd[post_s$y_ppd>0]
  }
  if (N == 0){
    sigma = rcauchy(10000, 3, 2)
    #sigma = sigma[idx_ok]
    mu = runif(10000, 1, 25)
    post_pred = rnorm(length(sigma), mu, sigma)
    post_pred = post_pred[!is.na(post_pred)]
    post_pred = post_pred[post_pred>0]
    sum(post_pred>0)
  }
  
  xlims <- c(1,25)
  ylims <- c(0,0.5)
  plot(density(post_pred), xlim=xlims, lwd=4, col=4 , 
       xlab="waiting time", main=paste0('Number of Datapoints ', N))
  if (N > 0) {
    rug(y[1:N], lwd=4)
    lines(density(old_post), xlim=xlims, lwd=4, col="#00000080")   
  }
  old_post = post_pred
}

################
# More than one Cafe
set.seed(7)
n_cafes <- 5
#w <- rpois(n_cafes,5)
n_visits <- 20
#cafe <- sample(1:n_cafes,size=n_visits,replace=TRUE)
#y <- rpois(n_visits,w[cafe])
#blank(bty="n",w=1.7)
ma <- cmdstan_model(here('stan','mixed_cafes','Cafes_of_Konstanz_non_centered.stan'), compile = TRUE )
ma <- cmdstan_model(here('stan','mixed_cafes','Cafes_of_Konstanz.stan'), compile = TRUE )
multi = TRUE
#Independend Model
ma <- cmdstan_model(here('stan','mixed_cafes','Cafes_of_Konstanz_indep.stan'), compile = TRUE )

multi = FALSE
ma$code()

n_cafes <- 5
w <- c(3,5,3,1,3)
set.seed(3)
n_visits <- 20
cafe <- c( rep(1,1) , rep(2,1) , 1 , rep(c(1,3,4,5),times=4) , 2 )
y <- rpois(n_visits,w[cafe]) + 1
y[2] <- 12

# loop
library(animation)
ani.record(reset=TRUE)
par(mfrow=c(2,3))
xlims <- c(0,15)
ylims <- c(0,0.4)
for (f in 0:19){ #n_visits ) {
    #f = 1
    post = NULL
    if ( f > 0 ) {
        m <- f
        dat <- list( N=m , n_cafes=n_cafes , y=y[1:m] , cafe=as.integer(cafe[1:m]) )
        max <- ma$sample(data = dat, chains = 4, 
                    parallel_chains = 1, iter_sampling = 1000, iter_warmup = 5000, 
                    adapt_delta = 0.9, max_treedepth = 15, 
                    save_warmup = FALSE )
        # Extract samples as df
        post$sigma = spread_draws(max, c(sigma))  %>% select(sigma)
        post$mu = spread_draws(max, c(mu[cafe])) %>%
          select(.draw, cafe, mu) %>%
          pivot_wider(
            id_cols = .draw,
            names_from = cafe,
            values_from = mu,
            names_prefix = "mu["
          ) %>%
          rename_with(~ paste0(., "]"), starts_with("mu[")) %>% 
          select(starts_with("mu[")) %>% as.matrix()
        
        if (FALSE){
          bayesplot::mcmc_trace(max$draws())
        }
    } else {
        # post$mu_bar <- runif(4000,1,30)
        # post$mu_delta <- rnorm(4000,0,5)
        # post$sigma <- rexp(4000,10)
        
        post$mu_bar <- runif(4000,1,25)
        post$mu_delta <- rnorm(4000,0,5)
        post$sigma <- rcauchy(4000, 3, 2)
        post$mu <- replicate(n_cafes, post$mu_bar)
    }

    # population distribution
    if (multi){
      if ( f == 0  ){
          #dens(post$mu_bar, xlim=xlims, lwd=4, col=4 , xlab="waiting time" , ylab="" , ylim=ylims, main="")
          plot( density(post$mu_bar), xlim=xlims, lwd=4, col=4 , xlab="waiting time" , ylab="" , ylim=ylims, main="")
          #hist(post$mu_bar, 20)
      }
      else {
          plot( density(post$mu_bar), xlim=xlims, lwd=4, col=4 , xlab="waiting time" , ylab="" , ylim=ylims, main="")
          lines(density(post_old$mu_bar), xlim=xlims, lwd=4, col="#00000080")   
      }
    } else{
      plot.new()
    }

    mtext(paste0("mu_bar total visits ", f))

    for ( j in 1:n_cafes ) {
        #j = 1
        xlwd <- 4
        sig = sqrt(mean(post$sigma$sigma^2))
        mu = mean(post$mu[,j])
        if ( f==0 )
            curve( dnorm(x,mu,sig) , from=0, to=20 , lwd=4, col=2 , xlab="waiting time" , ylab="" , ylim=ylims )
        else {
            if ( j==cafe[f] ) xlwd <- 8
            curve( dnorm(x,mu,sig) , from=0, to=20 , lwd=4, col="white" , xlab="waiting time" , ylab="" , ylim=ylims )
            curve( dnorm(x,mean(post_old$mu[j]),sig) , from=0, to=20 , lwd=4, col='gray' , add=TRUE )
            curve( dnorm(x,mu,sig) , from=0, to=20 , lwd=8, col="white" , add=TRUE )
            curve( dnorm(x,mu,sig) , from=0, to=20 , lwd=4, col=2 , add=TRUE )
            if ( j==cafe[f] ) lines( c(y[f],y[f]) , c(0,10) , lwd=4 , col=1 )
        }
        mtext(paste0( ifelse(f > 0, sum(cafe[1:f]==j),0) , " visits, mu=", round(mu,1) ))
    }
    post_old <- post
    ani.record()
}

oopts = ani.options(interval = 1)
ani.replay()
saveGIF(ani.replay(), movie.name = 'cafes_pooling.gif', dpi=250)
saveVideo(ani.replay(), movie.name = 'cafes_pooling.mp4', dpi=150)
#ani.saveqz(dpi=150)
# convert -alpha remove -background white -delay 8 -loop 0 frame*.png a_out.gif
# convert -delay 10 a_out.gif a_out.gif


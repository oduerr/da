#https://mc-stan.org/docs/2_21/stan-users-guide/logistic-probit-regression-section.html
#setwd("~/Documents/workspace/da/stan/logreg_challenger/")
library(ggplot2)
library(edudat)
challenger = load_data('challenger.csv')
#challenger$Temp = scale(challenger$Temp)
## Scaling the data
challenger$Temp = scale(challenger$Temp)
#Creating an empty plot
plot(1,xlim=c(-5,5), xlab='Temp', ylab='Failure', main='Challenger')
#rug(unscale(challenger$Temp), col='red', las=2)
rug(challenger$Temp, col='blue', las=2)


######### Max Likelihood #######
Temp = challenger$Temp
Failure = challenger$Failure

qplot(Temp, Failure)
erg<-glm(as.factor(Failure)~Temp, family=binomial(logit), data = challenger)
summary(erg)
a<-erg$coefficients[1]
b<-erg$coefficients[2]
x<-seq(-5,5,0.2)
#x<-seq(-5,5,0.1)
fit = data.frame(x = x, y = exp(a+b*x)/(1+exp(a+b*x)))
#library(ggstance)
df = data.frame(x=Temp, y=Failure)
ggplot(df) + 
  geom_point(aes(x=x,y=Failure),alpha=0.6) + 
  geom_line(data = fit, aes(x = x, y = y), col='blue')


#### Prior Analysis #####
#  y ~ bernoulli_logit(alpha + beta * x); 
#  z = alpha + beta * x;
#  alpha ~ normal(0,100);

sig = function(x) 1/(1+exp(-x))
alpha = rnorm(1e4,0.0,10)
png('stan/logreg_challenger/prior_10.png', width=500, height=1000)
#pdf('stan/logreg_challenger/prior_10.pdf', width=300, height=1000)
par(mfrow=c(3,1))
#Store png
hist(alpha, breaks = 100, freq = FALSE, col='red', xlab='alpha', main='alpha ~ N(0,10)', xlim=c(-30,30), cex.lab=3, cex.main=3)
curve(sig, from=-30, to=30, col='blue', lwd=2, xlim=c(-30,30))
hist(sig(alpha), breaks = 100, freq = FALSE, col='lightblue', xlab='p', cex.lab=3, cex.main=3)
#Store in pdf
dev.off()
par(mfrow=c(1,1))

alpha = rnorm(1e4,0.0,1.5)
png('stan/logreg_challenger/prior_1.5.png', width=500, height=1000)
#pdf('stan/logreg_challenger/prior_10.pdf', width=300, height=1000)
par(mfrow=c(3,1))
#Store png
hist(alpha, breaks = 100, freq = FALSE, col='red', xlab='alpha', main='alpha ~ N(0,1.5)', xlim=c(-5,5), cex.lab=3, cex.main=3)
curve(sig, from=-30, to=30, col='blue', lwd=2, xlim=c(-5,5))
hist(sig(alpha), breaks = 100, freq = FALSE, col='lightblue', xlab='p', cex.lab=3, cex.main=3)
#Store in pdf
dev.off()
par(mfrow=c(1,1))

alpha = rnorm(1e4,0.0,0.5)
png('stan/logreg_challenger/prior_0.5.png', width=500, height=1000)
#pdf('stan/logreg_challenger/prior_10.pdf', width=300, height=1000)
par(mfrow=c(3,1))
#Store png
hist(alpha, breaks = 100, freq = FALSE, col='red', xlab='alpha', main='alpha ~ N(0,0.5)', xlim=c(-5,5), cex.lab=3, cex.main=3)
curve(sig, from=-30, to=30, col='blue', lwd=2, xlim=c(-5,5))
hist(sig(alpha), breaks = 100, freq = FALSE, col='lightblue', xlab='p', cex.lab=3, cex.main=3)
#Store in pdf
dev.off()
par(mfrow=c(1,1))


######### Prior Predictive Simu ########
library(tidybayes)
library(magrittr)
library(dplyr)


x<-seq(-5,5,0.1)
prior_sd=1.5

library(cmdstanr)
options(mc.cores = parallel::detectCores())
# First Prior Predictive Check
m_rcmdstan <- cmdstan_model('stan/logreg_challenger/log_reg.stan')
data_prior = list(N=0,x=numeric(0),y=integer(0), N2=length(x), x2=x, prior_sd=prior_sd)
samples_N0 = m_rcmdstan$sample(iter_sampling = 1000,data=data_prior)
#View(head(samples_N0$draws(format = 'df')))

library(ggplot2)
library(tidybayes)
library(dplyr)
library(gridExtra)
library(gganimate)


# Function to create the plots
create_plots <- function(idx, samples, x, prior_sd, dat=NULL) {
  dd = spread_draws(samples, p_predict[i])
  
  solid_lines = dd %>% filter(.draw %in% idx)
  solid_lines$x = rep(x, each = length(idx))
  
  grey_lines = dd %>% filter(.draw %in% (1:50))
  grey_lines$x = rep(x, each = length(1:50))
  
  quantiles = dd %>%
    group_by(x = rep(x, each = length(unique(dd$.draw)))) %>%
    summarise(
      q05 = quantile(p_predict, 0.05),
      q95 = quantile(p_predict, 0.95)
    )
  
  g1 = ggplot(solid_lines, aes(x = x, y = p_predict)) +
    geom_line(aes(col = as.factor(.draw)), size = 2) +
    geom_line(data = quantiles, aes(x = x, y = q05), linetype = "dashed", color = "blue") +
    geom_line(data = quantiles, aes(x = x, y = q95), linetype = "dashed", color = "blue") +
    #geom_line(data = grey_lines, aes(x = x, y = p_predict, col = as.factor(.draw)), alpha = 0.1) +
    theme_minimal() + guides(color = "none")
  
  if (is.null(dat)){
    g1 = g1 + labs(title = paste0('Prior Predictive with N(0,', prior_sd, ')')) 
  } else{
    g1 = g1 + 
      geom_jitter(data = dat, aes(x = x, y = y), col = 'black', size = 3, width=0, height=0.05) +
      labs(title = paste0('Posterior Predictive with N(0,', prior_sd, '), N=', nrow(dat))) 
  }
  
  post = spread_draws(samples, alpha, beta)
  post_solid = post %>% filter(.draw %in% idx)
  
  g2 = ggplot(post, aes(x = alpha, y = beta)) +
    geom_point(col = 'grey', size = 0.2) +
    geom_point(data = post_solid, aes(x = alpha, y = beta, col = as.factor(.draw)), size = 3) +
    xlim(-5,5) + ylim(-5,5) + #<----- A bit hacky
    theme_minimal() + guides(color = "none")
  
  if (is.null(dat)){
    g2 = g2 + labs(title = paste0('Prior with N(0,', prior_sd, ')')) 
  } else{
    g2 = g2 + labs(title = paste0('Posterior with N(0,', prior_sd, '), N=', nrow(dat))) 
  }
  
  
  return(gridExtra::grid.arrange(g2, g1, ncol = 2))
}

if (FALSE){
  create_plots(idx, samples_N0, x, prior_sd)
}

frames = list()
for (i in 1:20){
  idx = i:(i+2)
  file_name <- sprintf("stan/logreg_challenger/tmp/frame_%03d.png", i)
  g = create_plots(idx, samples_N0, x, prior_sd)
  ggsave(file_name, plot = g, width = 10, height = 5)
}

image_path <- "stan/logreg_challenger/tmp"  # Replace with your actual path
image_files <- list.files(image_path, pattern = "frame_\\d{3}\\.png", full.names = TRUE)
library(av)
av_encode_video(frame_files, framerate = 1, 
                output = paste0("stan/logreg_challenger/animation_prior_sd", prior_sd, "N_0.mp4"),
                codec = "libx264"  # Use H.264 codec for better quality
                )


######### Posterior Predictive Simu ########
prior_sd = 1.5
challenger = load_data('challenger.csv')
x_launch = (31. - mean(challenger$Temp))/sd(challenger$Temp)
challenger$Temp = as.vector(scale(challenger$Temp))
for (N in 1:nrow(challenger)){
  #N = 23
  m_rcmdstan <- cmdstan_model('stan/logreg_challenger/log_reg.stan')
  dat = challenger[1:N,,drop=FALSE]
  dat = rename(dat, c('x'='Temp', 'y'='Failure'))
  data=list(N=nrow(dat),x=dat$x, y=dat$y, N2=length(x), x2=x, prior_sd=prior_sd)
  samples = m_rcmdstan$sample(iter_sampling = 1000,data=data)
  samples
  frames = list()
  for (i in 1:20){
    idx = i:(i+2)
    file_name <- sprintf("stan/logreg_challenger/tmp/frame_%03d.png", i)
    g = create_plots(idx, samples, x, prior_sd, dat)
    #plot(g)
    ggsave(file_name, plot = g, width = 10, height = 5)
  }
  
  image_path <- "stan/logreg_challenger/tmp"  # Replace with your actual path
  image_files <- list.files(image_path, pattern = "frame_\\d{3}\\.png", full.names = TRUE)
  library(av)
  av_encode_video(frame_files, framerate = 1, 
                  output = paste0("stan/logreg_challenger/animation_prior_sd", prior_sd, "N_", N, ".mp4"),
                  codec = "libx264"  # Use H.264 codec for better quality
  )
}

##### Posterior Predictive at Launch 31°#####
prior_sd = 100
challenger = load_data('challenger.csv')
m_rcmdstan <- cmdstan_model('stan/logreg_challenger/log_reg.stan')
#m_rcmdstan <- cmdstan_model('stan/logreg_challenger/log_reg_no_prior.stan')
challenger$Temp = as.vector(scale(challenger$Temp))
dat = challenger
data=list(N=nrow(dat),x=dat$Temp, y=dat$Failure, N2=length(x), x2=x, prior_sd=prior_sd)
samples = m_rcmdstan$sample(iter_sampling = 5000,data=data)
post = spread_draws(samples, alpha, beta)
smps = sig(post$alpha + post$beta*x_launch)
hist(smps, breaks=100, freq=FALSE, col='red', xlab='p', main='Posterior Predictive at Launch 31°')
quantile(smps, probs=c(0.05,0.95)) # 0.884, 0.999 with sd1.5 prio. With sd=100 prior 0.9602607 1.000
mean(smps) #0.977
mean(post$alpha)

##### No Prior --> Max Likelihood in optimization
m_rcmdstan <- cmdstan_model('stan/logreg_challenger/log_reg_prior_pred.stan') #Compiling
map = m_rcmdstan$optimize(data, jacobian=TRUE) #MAP
cat("glm ", a, b, ' Stan ', map$mle()[1:2], '\n')
samples = m_rcmdstan$sample(iter_sampling = 10000,data=data)
samples
mcmc = samples$draws(c('alpha', 'beta'), format='df')
mean(mcmc$alpha)
median(mcmc$alpha)

########### OLD STUFF ######

##### Prior
m_rcmdstan <- cmdstan_model('stan/logreg_challenger/log_reg.stan') #Compiling
map = m_rcmdstan$optimize(data, jacobian=TRUE) #MAP
cat("glm ", a, b, ' Stan ', map$mle()[1:2], '\n')


samples = m_rcmdstan$sample(iter_sampling = 2000,data=list(N=nrow(challenger),x=challenger$Temp, y=challenger$Failure, N2=length(x), x2=x))
bayesplot::mcmc_trace(samples$draws(c('alpha', 'beta', 'lp__')))
# density plot for alpha and beta and compare with the glm results
d = samples$draws('alpha', format='df')
plot(density(d$alpha))
mean(d$alpha)
median(d$alpha)


samples
d = samples$draws('p_predict', format='df')
# Remove all columns starting with '.' like 'chain...'
d = d[,!grepl('^\\.', colnames(d))]

plot(Temp, Failure, xlim=c(-5,5), main='Challenger', sub='Bayes vs ML') 
#pdf('res.png')
l = function(y_pred,x2, col='green'){
  m = apply(y_pred, 2,quantile, probs=c(0.50)) 
  lines(x2, m,col=col)
  q05 = apply(y_pred, 2, quantile, probs=c(0.30)) 
  q95 = apply(y_pred, 2, quantile, probs=c(0.70)) 
  lines(x2, q05,col=col)
  lines(x2, q95,col=col)
}
l(d,x2=x)
lines(fit$x, fit$y)
legend('topright', legend = c('Bayes','ML'), col=c('green','Max'), )

#dev.off()





if (FALSE){
  # STAN Version
  library('rstan')
  model = stan_model(file='stan/logreg_challenger/log_reg.stan')
  log.samples = sampling(model, list(N=23L,x=challenger$Temp, y=challenger$Failure, N2=length(x), x2=x))
  traceplot(log.samples)
  print(log.samples)
  d = extract(log.samples)
  rethinking::dens(d$alpha, main='alpha')
  d = "rstudio://open?file=/Users/oli/Documents/GitHub/da/stan/logreg_challenger/log_reg.stan"
}




# gif's are not so good when it comes to powerpoint presentations (no video control)
# library(magick)
# image_list <- lapply(image_files, image_read)
# animation <- image_animate(image_join(image_list), fps = 2)
# image_write(animation, paste0("stan/logreg_challenger/animation_prior_sd", prior_sd, ".gif"))


# num_intermediate_frames <- 10
# all_frames <- list()
# for (i in 1:(length(image_list) - 1)) {
#   all_frames <- append(all_frames, list(image_list[[i]]))
#   intermediate_frames <- image_morph(c(image_list[[i]], image_list[[i + 1]]), num_intermediate_frames)
#   all_frames <- append(all_frames, intermediate_frames)
# }
# all_frames <- append(all_frames, list(image_list[[length(image_list)]]))
# animation <- image_animate(image_join(all_frames), fps = 10)  # Adjust fps for smoother animation
# image_write(animation, paste0("stan/logreg_challenger/animation", prior_sd, ".gif"))



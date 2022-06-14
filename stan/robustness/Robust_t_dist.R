##### Testing for robustness #####
#See also https://solomonkurz.netlify.app/post/2019-02-02-robust-linear-regression-with-student-s-t-distribution/
library(dplyr)

#
nu = 2.5
tibble(z_score           = 0:-5,
       p_Gauss               = pnorm(z_score, mean = 0, sd = 1),
       p_Student_t           = pt(z_score, df = nu),
       `Student/Gauss ratio` = p_Student_t/p_Gauss,
       `Student/Gauss log-ratio` = log(p_Student_t/p_Gauss),
       ) %>%
  knitr::kable()



library(brms)
######## Creating Data #####
s <- matrix(c(1, .6, 
              .6, 1), 
            nrow = 2, ncol = 2)
set.seed(3)

#Data w/o outliers
d <- MASS::mvrnorm(n = 100, mu = c(0,0), Sigma = s) %>%
  as_tibble() %>%
  rename(y = V1, x = V2) %>%  
  arrange(x) #Sorts the data


#Maximum Likelihood approach to real data
plot(y ~ x, d)
lm.d = lm(y ~ x, d)#0.008822(Intercept)  0.610414(x) 
abline(lm.d)

#
o <- d
o[c(1:2), 1] <- c(5, 4.5)
### Maximum Likelihood Approach
lm.o = lm(y ~ x, o)#0.1516(Intercept)  0.2253(x) 
plot(y ~ x, o)
abline(lm.d, col='green')
abline(lm.o, col='red')

### Bayesian Approach
if (!exists("bayes_gauss")){
  bayes_gauss <- brm(data = o, family = gaussian, y ~ x)
}
b = posterior_summary(bayes_gauss)[1,1]
a = posterior_summary(bayes_gauss)[2,1]
curve(b + a * x,add=TRUE, col='blue', lty=3)
#mcmc_plot(bayes_gauss, type='trace_highlight') #Look good
#mcmc_plot(bayes_gauss, type='rhat') #Look good
bayes_gauss #ESS looks good, rhat very close to 1

if (!exists("bayes_stud")){
  bayes_stud <- brm(data = o, family = student, y ~ x)
}
bayes_stud #looks good
b = posterior_summary(bayes_stud)[1,1]
a = posterior_summary(bayes_stud)[2,1]
curve(b + a * x,add=TRUE, col='green', lty=3)

##### Model Comparision
loo_bayes_gauss = loo::loo(bayes_gauss)
loo_bayes_stud = loo::loo(bayes_stud)
loo_compare(loo_bayes_gauss, loo_bayes_stud)

#The Gaussmodel has a lower ELPD (the higher the better) 
#it is therefore rightly concluded that the bayes_stud model will perform better on unseen data

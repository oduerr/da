#### Refactoring of eval_performance.R but with cmdstanr
# Calculated performance of foorball prediction models
# Generated quantities block is used to calculate the log likelihood
# The log likelihood is used to calculate the LOO-CV

library(cmdstanr)
library(tidybayes)
library(dplyr)
library(magrittr)
options(mc.cores = parallel::detectCores())


####
set.seed(1) #set seed 
data = read.csv('https://raw.githubusercontent.com/MaggieLieu/STAN_tutorials/master/Hierarchical/premiereleague.csv',col.names = c('Home','score1', 'score2', 'Away'), stringsAsFactors = FALSE)
ng = nrow(data)
teams = unique(data$Home)
ht = unlist(sapply(1:ng, function(g) which(teams == data$Home[g])))
at = unlist(sapply(1:ng, function(g) which(teams == data$Away[g])))

cat('we have G =', ng, 'games \n')
nt = length(unique(data$Home))
cat('We have T = ', nt, 'teams \n')


#### getting data for STAN ########
# we will save the last np games to predict
get_data = function(np){
  ngob = ng-np #number of games to fit
  my_data = list(
    nt = nt, 
    ng = ngob,
    ht = ht[1:ngob], 
    at = at[1:ngob], 
    s1 = data$score1[1:ngob],
    s2 = data$score2[1:ngob],
    np = np,
    htnew = ht[(ngob+1):ng],
    atnew = at[(ngob+1):ng],
    s1new = data$score1[(ngob+1):ng],
    s2new = data$score2[(ngob+1):ng],
    
    score1 = data$score1, #Have all in one place
    score2 = data$score2
  )
  return (my_data)
}

# Function to extract the name
extract_name <- function(path) {
  file_name <- basename(path)  # Extract the file name with extension
  dir_path <- dirname(path)  # Extract the directory path
  
  # Extract the relevant part of the directory path
  # This assumes the structure "da/website/Euro24"
  relevant_part <- sub("^.*GitHub/", "", dir_path)
  
  # Combine the relevant part of the directory path with the file name without extension
  combined_name <- file.path(relevant_part, sub("\\.stan$", "", file_name))
  
  return(combined_name)
}

##################
# Get Score TODO FIX
get_score = function(post_samples, data){
  ngob = ng-np #number of games to fit
  #RMSE 
  pred_scores = c(colMeans(post_samples$s1new),colMeans(post_samples$s2new))
  true_scores = c(data$score1[(ngob+1):ng],data$score2[(ngob+1):ng] )
  acc = mean(round(pred_scores,0) == true_scores)
  rmse = sqrt(mean((pred_scores - true_scores)^2))
  cor = cor(pred_scores, true_scores)
  #Log-Score
  if (FALSE){
    plot(table(post_samples$s1new[,1]))
    mean(post_samples$s1new[,1])
    var(post_samples$s1new[,1])
  }
  
  
  s1_pred_score = round(colMeans(post_samples$s1new),0)
  s2_pred_score = round(colMeans(post_samples$s2new),0)
  points = 0
  nll_1 = 0
  nll_2 = 0
  for (i in 1:length(s1_pred_score)){
    ts1 = data$score1[ngob+i]
    ts2 = data$score2[ngob+i]
    nll_1 = nll_1 -log(1e-12 + mean(post_samples$s1new[,i] == ts1))
    nll_2 = nll_2 -log(1e-12 + mean(post_samples$s1new[,i] == ts2))
    pred_scorediff = mean(post_samples$s1new[,i] > post_samples$s2new[,i])
    if (s1_pred_score[i] == ts1 && s2_pred_score[i] == ts2){ #Exact
      points = points + 3
    } else {
      if (abs(pred_scorediff) < 0.5){ #Call it a draw
        if (ts1 == ts2) {
          points = points + 1
        }
      } else {#No draw
        if(sign(pred_scorediff) == sign(ts1 -ts2)){
          points = points + 1
        }
      }
    } 
  }
  return (list(score = points/(1.0*length(s1_pred_score)), rmse=rmse, cor=cor, nll_1 = nll_1/(1.0*length(s1_pred_score)), nll_2 = nll_2/(1.0*length(s1_pred_score))))
}

# calc_NLL <- function(s, d, ngob, m, res.df, ntrain) {
#   t1 = spread_draws(s, theta1new[i])
#   t2 = spread_draws(s, theta2new[i])
#   s1 = d$score1[(ngob+1):length(d$score1)] #Scores team 1 to predict
#   s2 = d$score2[(ngob+1):length(d$score2)] #Scores team 2 to predict
#   
#   ##### NLL on TestData #####
#   calc_nll <- function(observed, predicted) {
#     -mean(dpois(observed, predicted, log = TRUE))
#   }
#   
#   # Calculate NLL for each set of predictions
#   nll_values <- t1 %>%
#     group_by(i) %>%
#     summarize(nll = calc_nll(s1[i], theta1new)) 
#   
#   average_nll <- mean(nll_values$nll)
#   
#   nll_values <- t2 %>%
#     group_by(i) %>%
#     summarize(nll = calc_nll(s2[i], theta2new)) 
#   
#   average_nll <- average_nll + mean(nll_values$nll)
#   
#   log_lik = s$draws('log_lik') #extract the log likelihood of the training data
#   res = loo::loo(log_lik)
#   # Create Table with LOO results
#   NLL = -res$elpd_loo / dim(log_lik)[3]
#   
#   name = extract_name(m$stan_file())
#   res.df = rbind(res.df, data.frame(ntrain=ntrain, res=average_nll, type = 'NLL_PRED', name = name))
#   res.df = rbind(res.df, data.frame(ntrain=ntrain, res=NLL, type = 'NLL_PSIS', name = name))
#   return (res.df)
# }



###### Main Loop ##########
models = list()
models = append(models,cmdstan_model('~/Documents/GitHub/da/website/Euro24/hier_model_cor.stan'))
models = append(models,cmdstan_model('~/Documents/GitHub/da/website/Euro24/hier_model_cor_nocholsky.stan'))
models = append(models,cmdstan_model('~/Documents/GitHub/da/website/Euro24/hier_model.stan'))
                
                

ntrains = 10:320
ntrains = c(10,20,30,40,50,60)
ntrains = c(10,200)

res.df = NULL
for (ntrain in ntrains){
  #ntrain = 100  
  np = ng - ntrain
  d = get_data(np)
  ngob = ng-np
  cat('---------------------------------------------------\n')
  cat(paste0('Number of trainingsdata ', ntrains))
  cat('---------------------------------------------------\n')
  for (m in models){
    #m = models[1]
    name = extract_name(m$stan_file())
    s = m$sample(data=d)
    #res.df = calc_NLL(s, d, ngob, m, res.df, ntrain)
    
    # Calculate the log likelihood of the prediction
    lp = -spread_draws(s, log_lik_pred)
    res.df = rbind(res.df, data.frame(ntrain=ntrain, res=mean(lp$log_lik_pred), type = 'NLL_PRED', name =  name))
    
    # Calculate the log likelihood of the training data using PSIS-LOO
    log_lik = s$draws('log_lik') #extract the log likelihood of the training data
    res = loo::loo(log_lik)
    NLL = -res$estimates['elpd_loo', 1] / dim(log_lik)[3]
    res.df = rbind(res.df, data.frame(ntrain=ntrain, res=NLL, type = 'NLL_PSIS', name =  name))
  }
}

res.df





























  
  
  
  
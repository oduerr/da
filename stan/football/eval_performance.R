require(rstan)
setwd("~/Documents/workspace/da/stan/football")
set.seed(1) #set seed 
data = read.csv('https://raw.githubusercontent.com/MaggieLieu/STAN_tutorials/master/Hierarchical/premiereleague.csv',col.names = c('Home','score1', 'score2', 'Away'), stringsAsFactors = FALSE)
teams = unique(data$Home)
ht = unlist(sapply(1:ng, function(g) which(teams == data$Home[g])))
at = unlist(sapply(1:ng, function(g) which(teams == data$Away[g])))
ng = nrow(data)
cat('we have G =', ng, 'games \n')
nt = length(unique(data$Home))
cat('We have T = ', nt, 'teams \n')


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
    score1 = data$score1, #Have all in one place
    score2 = data$score2
  )
  return (my_data)
}

##################
# Get Score
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
    if (s1_pred_score[i] == ts1 && s2_pred_score[i] == ts2){
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

get_posteriors = function(post_samples, data){
  return (list(
    team_id = 1:data$nt,
    attack = colMeans(post_samples$att),
    attack_sd = sapply(1:nt, function(x) sd(post_samples$att[,x])),
    defense = colMeans(post_samples$def),
    defense_sd = sapply(1:nt, function(x) sd(post_samples$def[,x]))
  )
  )
}

require(loo)
get_waics = function(stanfit){
  
}


####### Loading the models #####
nh_mod <- stan_model('non_hier_model.stan')
h_mod <- stan_model('hier_model.stan')
h_nb_mod <- stan_model('hier_model_nb.stan')


models = c(nh_mod, h_mod)#, h_nb_mod)
ntrains = 10:320

####### Runs #####
res = NULL
post_res = NULL
for (ntrain in ntrains){
  np = ng - ntrain
  my_data = get_data(np)
  cat(paste0('Number of trainingsdata ', ntrains))
  for (m in models){
    samp = sampling(m, data=my_data)
    #For PSIS one needs to introduce generated quantities
    #log_lik1 <- loo::extract_log_lik(samp, parameter_name = 'lp__')
    post_samples = rstan::extract(samp)
    s = get_score(post_samples, my_data)
    res = rbind(res, data.frame(name = m@model_name, ntrain, s))  
    p = get_posteriors(post_samples, my_data)
    post_res = rbind(post_res, data.frame(name = m@model_name, ntrain, p))
  }
  #save(res, post_res, file='results_2.rda')
}

####### Results #####
library(dplyr)
library(tidyr)
#According to https://en.wikipedia.org/wiki/2019%E2%80%9320_Premier_League
#Matchday 30 (of 38) has been postponed
res %>% pivot_longer(cols = 3:7, names_to='meassure') %>%  
  filter(meassure %in% c('nll_1', 'nll_2', 'cor', 'score')) %>% 
  ggplot() + 
  geom_line(aes(x=ntrain, y=value, col=name, linetype=meassure)) + 
  xlab('Number of Games for Training the Models') +
  ylim(0, 2.5) +
  geom_line(x=300, y=0,1) + 
  #facet_wrap( ~ meassure) +
  labs(subtitle = 'Comparison of hier. and non-hierchical model depending on training data',
  title = '2019/20 Premier League') +
  ggthemes::theme_clean()
ggsave('performance_w_nll.png')

if (FALSE){
  #Creation of the video: https://youtube.com/shorts/kUeuA__U1Ak?feature=share 
  #The gif has been transform to mov via imovie
  library(gganimate)
  library(transformr)
  teams = unique(data$Home)
  teams[!teams %in% 
          c("Manchester United", "Liverpool", "Manchester City","West Ham United", "Arsenal","Everton")] = NA
  d4p = post_res %>% left_join(tibble(team_id=1:20, teams)) 
  max(d4p$ntrain)
  frames = seq(10,320,1)
  p = d4p %>% filter(ntrain %in% frames) %>% #%>% filter(ntrain == 300) %>% 
    ggplot(aes(x=attack, y=defense, col=name)) + 
    geom_line(aes(group = team_id), col='gray', size = 1/4) + 
    geom_point(size=2) + geom_text(aes(label=teams)) + 
    coord_cartesian(xlim=c(-0.5,0.75), ylim=c(-0.5,0.75)) + 
    ggthemes::theme_clean() + theme(legend.position=c(0.15,0.9))
  #p
  m = p + transition_time(ntrain) + labs(title = "Games in Training: {frame_time}")
  a = animate(m, nframes = length(frames))
  #anim_save('attack_defense_shrinking.mp4',m) #does not provide a valid mp4
  anim_save('attack_defense_shrinking.gif',a)
}

if (FALSE){
  for (t in c(10,11,190)) {#unique(sort(post_res$ntrain))){
    p = post_res %>% 
      filter(ntrain == t) %>% 
      left_join(tibble(team_id=1:20, teams)) %>% 
      ggplot(aes(x=attack, y=defense, col=name)) + 
      geom_line(aes(group = team_id), col='gray', size = 1/4) + 
      geom_point() + geom_text(aes(label=teams)) + 
      labs(title = paste0('Games for Training: ', t), col='black') + 
      xlim(-1.,1) + ylim(-1,1) + ggthemes::theme_clean()
    p
    ani.record()
  }
  ani.replay()
}


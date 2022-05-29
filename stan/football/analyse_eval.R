load('results_2.rda')
library(ggplot2)
ggplot(res) + geom_line(aes(x=ntrain, y=nll_2, col=name))  

library(dplyr)
post_res %>% filter(ntrain == 20) %>% 
  ggplot(aes(x=attack, y=defense, col=name)) + geom_point() + geom_text(aes(label=team_id))

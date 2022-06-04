library(ggplot2)
library(dplyr)
library(magrittr)

############ Log and Exp ##############
xs  = seq(-3,3,0.01) 
xss = seq(0.01,3,0.01)
d = data.frame(x = xss, vals=log(xss));d$f='log';
dd = data.frame(x = xs, vals=exp(xs));dd$f='exp';
rbind(d, dd) %>% 
 ggplot(aes(x=x, y=vals, col=f)) +
  geom_line(size=2) + 
  geom_line(aes(x=x, y=x), col='gray', size=0.5) +
  ylab(latex2exp::TeX(r'($f(x)$)')) +
  coord_cartesian(xlim = c(-2.5, 2.5), ylim = c(-2.5, 2.5)) +
  theme_linedraw(base_size = 22) +
  theme(legend.position = c(0.2,0.82)) + 
  theme(legend.title = element_blank()) +
  guides(color=guide_legend(override.aes=list(fill=NA)))
  
ggsave('exp_log.svg', width = 5, height = 5)
ggsave('exp_log.pdf', width = 5, height = 5)

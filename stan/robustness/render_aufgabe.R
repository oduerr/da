#setwd("~/Documents/workspace/da/stan/robustness")
library(rmarkdown)
lsg = FALSE
rmarkdown::render('robustness.Rmd', output_format = 'github_document', output_file = 'robustness_no_lsg')
lsg = TRUE
rmarkdown::render('robustness.Rmd', output_format = 'github_document', output_file = 'robustness_lsg')


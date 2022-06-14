#setwd("~/Documents/workspace/da/stan/robustness")
library(rmarkdown)
lsg = FALSE
rmarkdown::render('rent_kn.Rmd', output_format = 'github_document', output_file = 'rent_kn_no_lsg')
lsg = TRUE
rmarkdown::render('rent_kn.Rmd', output_format = 'github_document', output_file = 'rent_kn_lsg')


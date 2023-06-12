library(rmarkdown)
library(tools)
lsg = FALSE
render(input = '/Users/oli/Documents/GitHub/da/lab/kn_rent/rent_kn_cmdrstan.Rmd', 
       output_format = 'github_document', output_file = '/Users/oli/Documents/GitHub/da/lab/kn_rent/rent_kn_cmdrstan_nolsg.md')

lsg = TRUE
render(input = '/Users/oli/Documents/GitHub/da/lab/kn_rent/rent_kn_cmdrstan.Rmd', 
       output_format = 'github_document', output_file = '/Users/oli/Documents/GitHub/da/lab/kn_rent/rent_kn_cmdrstan_lsg.md')

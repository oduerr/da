library(ggplot2)
library(dplyr)
library(magrittr)
library(latex2exp)

# Generate x values
xs <- seq(-5, 5, 0.01)

# Compute PDF and CDF of the standard normal distribution
pdf_vals <- dnorm(xs)
cdf_vals <- pnorm(xs)

# Create data frames
pdf_data <- data.frame(x = xs, vals = pdf_vals, f = 'PDF')
cdf_data <- data.frame(x = xs, vals = cdf_vals, f = 'CDF')

# Combine data and set factor levels to control order
data_combined <- rbind(pdf_data, cdf_data)
data_combined$f <- factor(data_combined$f, levels = c("PDF", "CDF"))  # Ensures PDF is first

# Plot
ggplot(data_combined, aes(x = x, y = vals, col = f)) +
  geom_line(size = 2) +
  ylab(TeX(r'($f(x)$ or $F(x)$)')) +
  theme_linedraw(base_size = 22) +
  #theme(legend.position = c(0.2, 0.82)) +
  #theme(legend.title = element_blank()) +
  guides(color = guide_legend(override.aes = list(fill = NA))) +
  facet_wrap(~f, scales = "free_y", ncol = 2)  # Forces two columns with PDF first

# Save as file
ggsave('gaussian_pdf_cdf.svg', width = 6, height = 4)
ggsave('gaussian_pdf_cdf.pdf', width = 6, height = 4)
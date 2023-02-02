library(cmdstanr)
df = list(
  N = 4,
  x = c(-2.,-0.66666, 0.666, 2.),
  y = c(-6.25027354, -2.50213382, -6.07525495,  7.92081243)
)
m_rcmdstan <- cmdstan_model('simple_lr.stan')
s_rcmdstan = m_rcmdstan$sample(data = df)


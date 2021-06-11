library(rethinking)

n = 1000
a = rnorm(n, mean=1)
b = rnorm(n, mean=2, sd=2)
y = rnorm(a * b, sd=1)
hist(y,100)


def = alist(
  y ~ dnorm(a*b, sig),
  a ~ dnorm(a_mu, a_sig),
  b ~ dnorm(b_mu, b_sig),
  c(sig, a_sig, b_sig) ~ dexp( 1 ),
  c(a_mu, b_mu) ~ dnorm(0, 1 )
)

dat_list <- list(y= y)

mod1 <- ulam(def, data=dat_list , chains=4 , cores=4 , iter=2000 )
precis( mod1 , depth=3 )
#       mean   sd  5.5% 94.5% n_eff Rhat4
#a     -0.04 0.52 -0.83  0.80   610  1.01
#b     -0.02 0.51 -0.76  0.76   596  1.01
#b_sig  0.68 0.75  0.01  2.13   102  1.05
#a_sig  0.68 0.74  0.05  2.10   132  1.05
#sig    1.02 0.02  0.98  1.05   643  1.02
#b_mu  -0.01 0.65 -1.01  1.06   881  1.01
#a_mu  -0.02 0.64 -1.01  1.03  1012  1.01


n = 1000
a = rnorm(n, mean=1)
b = dexp(n, rate=2)
y = rnorm(a * b, sd=1)
hist(y,100)


def2 = alist(
  y ~ dnorm(a*b, sig),
  a ~ dnorm(a_mu, a_sig),
  b ~ dexp(b_rate),
  c(sig, a_sig, b_rate) ~ dexp( 1 ),
  a_mu ~ dnorm(0, 1 )
)

dat_list <- list(y= y)

mod1 <- ulam(def2, data=dat_list , chains=4 , cores=4 , iter=2000 )
precis( mod1 , depth=3 )
par(mfrow=c(1,1))
xs = seq(0,10,length.out=100)
plot(xs, dlnorm(xs, meanlog = 0.5, sdlog = 1), type='l')
lines(xs, dlnorm(xs, meanlog = 1, sdlog = 1), type='l')
plot(xs, dlnorm(xs, meanlog = 1.2, sdlog = 1), type='l')

plot(xs, dcauchy(xs, 1, 2), type='l')


plot(xs, dlnorm(xs, meanlog = 2, sdlog = 5), type='l')
lines(xs, dlnorm(xs, meanlog = 5, sdlog = 1), type='l')
lines(xs, dlnorm(xs, meanlog = 5, sdlog = 2), type='l')

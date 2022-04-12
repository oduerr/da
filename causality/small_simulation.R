N = 10000
#The Exogenous variables
UZ = rnorm(N)
UX = rnorm(N)
UY = rnorm(N)
UW = rnorm(N)
UW2 = rnorm(N)

Z = UZ
X = 0.1*Z + UX
Y = 0.42*X + 0.8* Z + UY
W = 1.1 * X - 0.6*Y + UW
W2 = 0.2*W + UW2

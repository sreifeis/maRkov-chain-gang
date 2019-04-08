# Simulate data to test Lasso function

# Number of samples
n = 100

# Pick true Beta parameters
beta = c(-2.5, 1, -2, 3, -4, numeric(10))
p = length(beta)

covar = matrix(rnorm(100*(p-1)), nrow = n, ncol = p-1)

X = cbind(rep(1,n), covar)
prob = exp(X%*%beta) / (1 + exp(X%*%beta))
y = rbinom(n, 1, prob)



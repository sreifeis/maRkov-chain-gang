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

# Using 761 notes, find lambda_max

lam_option = numeric(ncol(X))
for(j in 1:ncol(X)){
  lam_option[j] = (1/nrow(X)) * abs(t(X[,j]) %*% y)
}

lambda_max = max(lam_option)
epsilon = 0.001
lambda_min = lambda_max * epsilon

# Recommendation by 761 notes: perform sequence on log scale
log_lam = seq(from = log(lambda_max), to = log(lambda_min), by = -0.05)

###################################################

# Testing Hillary's rough draft code

# For lambda_max, B = 0 for all j
# For subsequent lambda, use previous B as initial values
B = numeric(length = ncol(X))
results = list()
lambda = exp(log_lam)

for(l in 2:length(lambda)){
  results[[l]] = penal(y, X, lambda[l], B, family = "binomial") 
  B = results[[l]]$B_new
  
}


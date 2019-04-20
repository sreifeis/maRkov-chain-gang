# Simulate data to test Lasso function
# Simulation only section:

    # Number of samples
    n = 100
    
    # Pick true Beta parameters
    beta = c(-2.5, 1, -2, 3, -4, numeric(10))
    p = length(beta)
    
    covar = matrix(rnorm(100*(p-1)), nrow = n, ncol = p-1)
    
    X = cbind(rep(1,n), covar)
    prob = exp(X%*%beta) / (1 + exp(X%*%beta))
    y = rbinom(n, 1, prob)
    
######################################################
    
# Define y and X in our data
    
# y = nx1 vector, X = nxp matrix with intercept column
    
######################################################

# Find maximum possible lambda. At this value, B vector = 0

library(stats) # For 'glm'
library(stats4) # For BIC calculation
fit = glm(y ~ 1, family = "binomial")
# Extract intercept
int = fit$coefficients
B_test = c(int, numeric((ncol(X) - 1)))

prob = exp(int) / (1 + exp(int))

# Find lambda_max
W = diag(prob*(1-prob), nrow = n) # Weight matrix when B = 0 (other than intercept)
y_wr = (y - prob) / (prob * (1-prob)) + X %*% B_test # Working response for intercept-only results

lam_option = numeric(ncol(X))
for(j in 1:ncol(X)){
  lam_option[j] = (1/nrow(X)) * abs(t(X[,j]) %*% W %*% (y_wr - X[,-j] %*% B_test[-j]))
}

lambda_max = max(lam_option)
epsilon = 0.001
lambda_min = lambda_max * epsilon

# Recommendation by 761 notes: perform sequence on log scale
log_lam = seq(from = log(lambda_max), to = log(lambda_min), by = -0.05)
# Re-transform back to regular scale
lambda = exp(log_lam)

###################################################

# Run lasso

# Specify initial B = 0 (due to first lambda = maximum lambda)
B = numeric(length = ncol(X))
# Initialize list that will store the B vectors for each lambda
results = list()
# For lambda max, B = 0
results[[1]] = B
# Initialize vector to store BIC values
bic = numeric(length(lambda))
bic[1] = 10^10 # Some arbitrarily large value

for(l in 2:length(lambda)){
  # Calculate new B for specified lambda
  results[[l]] = LQA_lasso(X, Y, B, lambda[l], tol = 10^-5, max_it = 1000) # Add "family = 'binomial'"
  # In above: will need to switch with wrapper R function
  # Update B with new value
  B = results[[l]]
  # Calculate BIC
    # Calculate linear predictor with updated B
    eta = X %*% B
    # BIC = -2 log-lik + (num parameters) * log(num observations)
    bic[l] = -2 * sum( y * eta - log(1 + exp(eta)) ) / nrow(X) +
      sum(B!=0) * log(nrow(X))
}

# BIC isn't really working right now, but general idea:
which.min(bic)
results[[which.min(bic)]]

# Alternative BIC - need to work through issues
fit.bic = glm(y ~ , family = "binomial", offset = eta)
BIC(fit.bic)

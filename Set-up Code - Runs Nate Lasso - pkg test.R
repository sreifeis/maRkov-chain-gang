## SAR: code copied and slightly modified from 'Set-up Code - Runs Nate Lasso.R'
##      to test functions in staRz package

## Set seed -- SAR added
set.seed(1) # seed for data generation

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

## Source Nate's cpp code -- SAR added 
library(Rcpp)
sourceCpp("Lasso_Rcpp_Nate.cpp")

# Run lasso

# Specify initial B = 0 (due to first lambda = maximum lambda)
B = numeric(length = ncol(X))
# Initialize list that will store the B vectors for each lambda
results = list()
# For lambda max, B = 0
results[[1]] = B
# Initialize vector to store BIC values
ebic = numeric(length(lambda))
ebic[1] = 10^10 # Some arbitrarily large value

for(l in 2:length(lambda)){
  # Calculate new B for specified lambda
  results[[l]] = LQA_lasso(X, Y=y, B, lambda[l], tol = 10^-5, max_it = 1000) # Add "family = 'binomial'"
  # In above: will need to switch with wrapper R function
  ## SAR Note: switched Y to y in line above
  # Update B with new value
  B = results[[l]]
  # Calculate BIC
  # Calculate linear predictor with updated B
  eta = X %*% B
  # EBIC = -2 log-lik + (num params) * log(num observations) + 2 * (num params) * constant * log(num total param - cols of X)
  ebic[l] = -2 * sum( y * eta - log(1 + exp(eta)) ) / nrow(X) +
    sum(B!=0) * log(nrow(X)) + 2 * sum(B!=0) * 0.5 * log(ncol(X))
  # Potential options for constant in last term: 0.25, 0.5, 1
  # According to Chen and Chen paper on extended BIC (EBIC)
  # 0.5 good option (1 if really concerned about FDR)
}

# BIC isn't really working right now, but general idea:
which.min(ebic)
results[[which.min(ebic)]]



############################################
## Run LQA.lasso function from staRz
############################################

## Load staRz pkg
library(staRz)

## Run Lasso using R wrapper function from staRz

# Specify initial B = 0 (due to first lambda = maximum lambda)
B.pkg = numeric(length = ncol(X))
# Initialize list that will store the B vectors for each lambda
results.pkg = list()
# For lambda max, B = 0
results.pkg[[1]] = B.pkg
# Initialize vector to store BIC values
bic.pkg = numeric(length(lambda))
bic.pkg[1] = 10^10 # Some arbitrarily large value

for(l in 2:length(lambda)){
  # Calculate new B for specified lambda
  results.pkg[[l]] = staRz::LQA.lasso(X, Y=y, B.pkg, lambda[l], tol = 10^-5, max_it = 1000) # Add "family = 'binomial'"
  # In above: will need to switch with wrapper R function
  ## SAR Note: switched Y to y in line above
  # Update B with new value
  B.pkg = results.pkg[[l]]
  # Calculate BIC
  # Calculate linear predictor with updated B
  eta.pkg = X %*% B.pkg
  # EBIC = -2 log-lik + (num params) * log(num observations) + 2 * (num params) * constant * log(num total param - cols of X)
  ebic.pkg[l] = -2 * sum( y * eta.pkg - log(1 + exp(eta.pkg)) ) / nrow(X) +
    sum(B.pkg!=0) * log(nrow(X)) + 2 * sum(B.pkg!=0) * 0.5 * log(ncol(X))
  # Potential options for constant in last term: 0.25, 0.5, 1
  # According to Chen and Chen paper on extended BIC (EBIC)
  # 0.5 good option (1 if really concerned about FDR)
}

## Check R wrapper function output against sourced cpp function output
all.equal(bic, bic.pkg) # close, but not identical


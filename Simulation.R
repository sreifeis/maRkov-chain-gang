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

results[[length(lambda)]]
results[[10]]

########################################################
# Testing penal function - issue with not updating B properly.

lam = lambda[100]
j = 2

# calc function
# Calculates updated B[j], used in "penal" function below

# Define working response, weights, and current-iteration residuals
eta = X %*% B # Linear predictor in terms of current values of B
p = exp(eta) / (1 + exp(eta)) # pi for current values, used in weights and working reponse
W = diag(as.numeric(p*(1-p))) # Make weights into diagonal matrix
y_wr = (y - p) / (p * (1-p)) + X %*% B # Working response
# res = solve(W) %*% (y - p) # residuals based on current values
v_j = (1/nrow(X)) * t(X[,j]) %*% W %*% X[,j]
# z_j = (1/nrow(X)) * t(X[,j]) %*% W %*% res + v_j * B[j]
# Alternatively:
z_j = (1/nrow(X)) * t(X[,j]) %*% W %*% (y_wr - X[,-j] %*% B[-j])

# Use above values to solve for next iteration value of Bj
if(z_j > 0 & lam < abs(z_j)){
  b_j = (z_j - lam) / v_j
}else if(z_j < 0 & lam < abs(z_j)){
  b_j = (z_j + lam) / v_j
}else{
  b_j = 0
}

# penal function

    # Define B0 = B from last iteration - use in convergence calculation
    B0 = B
    
    for(j in 1:ncol(X)){
      
      if(iter == 0){ # Update all B[j]
        b_j = calc(y, X, B, lam, j)
        
      }else{ # Only update non-zero B[j] after first round
        
        if(B[j] != 0){
          b_j = calc(y, X, B, lam, j)
        }else{
          b_j = 0
        }
      }
    
      B[j] = b_j
    }
    
    # Evaluate convergence criteria - Euclidean distance
    eps = sqrt((B - B0)^2)
    
    # Update iterations
    iter = iter + 1
    if(iter == maxit){warning("Model failed to converge within maximum number of iterations")}
    
  
  # Evaluate BIC for resulting model
  # From library(stats4): BIC function OR
  # BIC = -2 (log-lik) + n_param * log(n_obs)
  p_vec = exp(X %*% B)/(1+exp(X %*% B))
  BIC = -2 * sum( y * log(p_vec) + (1-y) * log(1-p_vec) ) + ncol(X) * log(nrow(X))
  
  # Return updated B and BIC criteria for each lambda
  return(list(lambda = lam, B_new = B, crit = BIC))
  
}

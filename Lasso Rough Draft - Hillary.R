# Outline for Lasso

library(stats4)
library(RcppArmadillo)

# Set-up: Need to define/create y, X, lambda (penalization parameters)
# Assumption: X includes intercept column (all 1s)

calc = function(y, X, B, lam, j){
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
  
  return(b_j)
}

penal = function(y, X, lam, B, family = "binomial"){
  if(!is.element(family, c("binomial","binom"))){
    stop("'family' must be 'binomial' or 'binom'")
  }
  # y = response vector
  # X = covariate matrix (n x p)
  # lam = penalization parameter
  # B = previous iteration value of beta vector
  
  # Define convergence criteria
  eps = -Inf
  tol = 10^-5
  maxit = 500 # Do we need a max number of iterations? Perhaps at least until we know the code works.
  iter = 0
  
  while(eps > tol & iter < maxit){
    # Define B0 = B from last iteration - use in convergence calculation
    B0 = B
    B_update = numeric(length(B))
    
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
      
      B_update[j] = b_j
    }
    
    # Evaluate convergence criteria - Euclidean distance
    eps = sqrt((B_update - B0)^2)
    
    # Update iterations
    iter = iter + 1
    if(iter == maxit){warning("Model failed to converge within maximum number of iterations")}
    
  } 
  
  # Evaluate BIC for resulting model
  # From library(stats4): BIC function OR
  # BIC = -2 (log-lik) + n_param * log(n_obs)
  p_vec = exp(X %*% B_update)/(1 + exp(X %*% B_update))
  BIC = -2 * sum( y * log(p_vec) + (1-y) * log(1-p_vec) ) + ncol(X) * log(nrow(X))
  
  # Return updated B and BIC criteria for each lambda
  return(list(lambda = lam, B_new = B_update, crit = BIC))
  
}

##########################################

# Find lambda_max and lambda_min
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

# In final results list, find B and lambda combo with the lowest BIC

# Run simulation in `Simulation.R` first
B = numeric(length = ncol(X))
results = list()
lambda = exp(log_lam)
results[[1]] = list(`lambda` = lambda[1], B_new = B, crit = Inf)

for(l in 2:length(lambda)){
  # B = results[[l-1]]$B_new
  results[[l]] = penal(y, X, lambda[l], B, family = "binomial") 
  B = results[[l]]$B_new
}


# penal = function(y, X, lam, B, family = "binomial", iter){
#   if(!is.element(family, c("binomial","binom"))){
#     stop("'family' must be 'binomial' or 'binom'")
#   }
#   # y = response vector
#   # X = covariate matrix (n x p)
#   # lam = penalization parameter
#   # B = previous iteration value of beta vector
#   
#   B_update = numeric(length(B))
#   
#   for(j in 1:ncol(X)){
#     
#     if(iter == 0){ # Update all B[j]
#       b_j = calc(y, X, B, lam, j)
#     }else{ # Only update non-zero B[j] after first round
#       if(B[j] != 0){
#         b_j = calc(y, X, B, lam, j)
#       }else{
#         b_j = 0
#       }
#     }
#     
#     B_update[j] = b_j
#   }
#   
#   return(B_update)
# }
# 
# # For lambda_max, B = 0 for all j
# # For subsequent lambda, use previous B as initial values
# B = numeric(length = ncol(X))
# results = list()
# lambda = exp(log_lam)
# 
# tol = 10^-5
# maxit = 500
# 
# for(l in 2:length(lambda)){
#   
#   # Define convergence criteria
#   eps = -Inf
#   iter = 0
#   lam = lambda[l]
#   
#   while(eps > tol & iter < maxit){
#     # Define B0 = B from last iteration - use in convergence calculation
#     B0 = B
#     
#     B_update = penal(y, X, lam, B, family = "binomial", iter)
#     
#     # Evaluate convergence criteria - Euclidean distance
#     eps = sqrt((B_update - B0)^2)
#     
#     # Update iterations
#     iter = iter + 1
#     if(iter == maxit){warning("Model failed to converge within maximum number of iterations")}
#     
#   } 
#   
#   # Evaluate BIC for resulting model
#   # From library(stats4): BIC function OR
#   # BIC = -2 (log-lik) + n_param * log(n_obs)
#   p_vec = exp(X %*% B_update)/(1 + exp(X %*% B_update))
#   BIC = -2 * sum( y * log(p_vec) + (1-y) * log(1-p_vec) ) + ncol(X) * log(nrow(X))
#   
#   # Record updated B and BIC criteria for each lambda
#   results[[l]] = list(lambda = lambda, B_new = B_update, crit = BIC)
#   # Update B with B_update results
#   B = results[[l]]$B_new
#   
# }
#   
#   
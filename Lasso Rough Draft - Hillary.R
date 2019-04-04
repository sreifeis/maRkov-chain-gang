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
  eps = Inf
  tol = 10^-5
  maxit = 500 # Do we need a max number of iterations? Perhaps at least until we know the code works.
  iter = 0
  
  while(eps > tol & iter < maxit){
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
    eps = sqrt(sum((B - B0)^2))
    
    # Update iterations
    iter = iter + 1
    if(iter == maxit){warning("Model failed to converge within maximum number of iterations")}
    
  } 
  
  # Evaluate BIC for resulting model
  # From library(stats4): BIC function OR
  # BIC = -2 (log-lik) + n_param * log(n_obs)
  # p_vec = exp(X %*% B)/(1 + exp(X %*% B))
  eta_vec = X %*% B
  BIC = -2 * sum( y * eta_vec - log(1 + exp(eta_vec)) ) / nrow(X) + sum(B != 0) * log(nrow(X))
  
  # Return updated B and BIC criteria for each lambda
  return(list(lambda = lam, B_new = B, crit = BIC))
  
}

##########################################

# Fit intercept-only model
# library(stats)
fit.int = glm(y ~ 1, family = "binomial")
int = fit.int$coefficients

# Result: intercept = -0.8473
prob = exp(int) / (1 + exp(int))

# Find lambda_max and lambda_min
W = diag(prob, nrow = n) # Weight matrix when B = 0

lam_option = numeric(ncol(X))
for(j in 1:ncol(X)){
  lam_option[j] = (1/nrow(X)) * abs(t(X[,j]) %*% W %*% (y - X[,-j] %*% B[-j]))
}

lambda_max = max(lam_option)
epsilon = 0.001
lambda_min = lambda_max * epsilon

# Recommendation by 761 notes: perform sequence on log scale
log_lam = seq(from = log(lambda_max), to = log(lambda_min), by = -0.05)
# log_lam = seq(from = log(3), to = log(0.005), by = -0.05)

###################################################

# Testing Hillary's rough draft code

# In final results list, find B and lambda combo with the lowest BIC

# Run simulation in `Simulation.R` first
B = numeric(length = ncol(X))
results = list()
lambda = exp(log_lam)
results[[1]] = list(`lambda` = lambda[1], B_new = B, crit = Inf)

bic.vec = numeric(length(lambda))
bic.vec[1] = 10^10

for(l in 2:length(lambda)){
  # B = results[[l-1]]$B_new
  results[[l]] = penal(y, X, lambda[l], B, family = "binomial") 
  B = results[[l]]$B_new
  bic.vec[l] = results[[l]]$crit
}

which.min(bic.vec)
results[[which.min(bic.vec)]]
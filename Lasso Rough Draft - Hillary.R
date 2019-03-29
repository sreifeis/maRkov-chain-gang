# Outline for Lasso

# Set-up: Need to define/create y, X, lambda (penalization parameters)

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
  tol = 10^-6
  maxit = 500 # Do we need a max number of iterations? Perhaps at least until we know the code works.
  iter = 0
  
  while(eps > tol & iter < maxit){
    # Define B0 = B from last iteration
    B0 = B
    
    for(j in 1:ncol(X)){
      # Define working response, weights, and current-iteration residuals
      eta = X %*% B0 # Linear predictor in terms of current values of B
      p = exp(eta) / (1 + eta) # pi for current values, used in weights and working reponse
      W = diag(p(1-p)) # Make weights into diagonal matrix
      y_wr = (y - p) / (p (1-p)) + X %*% B0 # Working response
      res = solve(W)(y - p) # residuals based on current values
      v_j = (1/n) * t(X[,j]) %*% W %*% X[,j]
      z_j = (1/n) * t(X[,j]) %*% W %*% res + v_j * B0[j]
      
      # Use above values to solve for next iteration of B
      if(z_j > 0 & lam < abs(z_j)){
        B[j] = (z_j - lam) / v_j
      }else if(z_j < 0 & lam < abs(z_j)){
        B[j] = (z_j + lam) / v_j
      }else{
        B[j] = 0
      }
      
    }
    
    # Evaluate convergence criteria - Euclidean distance
    eps = sqrt((B - B0)^2)
    
    # Update iterations
    iter = iter + 1
    if(iter == maxit){warning("Model failed to converge within maximum number of iterations")}
    
  } 
  
  # Evaluate BIC for resulting model
  
  
  # Return updated B and BIC criteria for each lambda
  return(list(lambda = lam, B_new = B, crit = BIC))
  
}

# Find lambda_max and lambda_min
lam = seq(from = lambda_max, to = lambda_min, by = -0.01)
# For lambda_max, set initial value of B = vector of 0s
B = numeric(ncol(X))
results = list()

for(l in 2:length(lam)){
    results[l] = penal(y, X, lam[l], B, family = "binomial") # results[[l]] ?
    B = results$B_new
  
}

# In final results list, find B and lambda combo with the lowest BIC
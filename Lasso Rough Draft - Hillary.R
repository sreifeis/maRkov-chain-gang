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
  
  # Set current iteration B0 equal to imported B
  B0 = B
  
  # Define working response, weights, and current-iteration residuals
  # ...
  
  # Define convergence criteria
  eps = -Inf
  tol = 10^-6
  maxit = 500 # Do we need a max number of iterations? Perhaps just until we know the code works.
  iter = 0
  
  while(eps > tol & iter < maxit)
  for(j in 1:ncol(X)){
    
  }
  
  # Return updated B and convergence criteria for each lambda
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
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
  
  
  
}
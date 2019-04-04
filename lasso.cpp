#define RCPP_ARMADILLO_RETURN_COLVEC_AS_VECTOR
#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

// [[Rcpp::export]]
double update(arma::vec y, arma::mat X, arma::vec B, double lam, unsigned int j){
  unsigned int n = X.n_rows;
  // unsigned int p = X.n_cols;
  double b_j;
  arma::vec prob(n);
  // arma::vec X_j = X.col(j)
  // arma::colvec X_j_col = X.col(j);
  // arma::rowvec X_j_col_trans = X_j_col.t();
  
  arma::vec eta = X * B;
  arma::vec mu = exp(eta) / (arma::ones(n) + exp(eta));
  arma::mat W = diagmat(mu);
  arma::vec y_wr = (y - mu) / (mu % (arma::ones(n)-mu)) + X * B ;
  
  // Rprintf("mu[0]: %f, W[1,1]: %f, y_wr[0]: %f \n", mu(0), W(1,1), y_wr(0));
  
  // arma::vec res = W.i() * (y - mu) ;
  double v_j;
  v_j = as_scalar(X.col(j).t() * W.i() * X.col(j)) / n;
  // double z_j = (1 / n) * as_scalar(X.col(j).t() * W.i() * res) + v_j * B(j);
  arma::mat X_noj = X;
  arma::mat B_noj = B;
  X_noj.shed_col(j);
  B_noj.shed_row(j);
  double z_j ;
  z_j = as_scalar(X.col(j).t() * W.i() * (y_wr - X_noj * B_noj)) / n;
  
  // Lasso solution: soft thresholding penalty
  if(z_j > 0 && lam < abs(z_j)){
    b_j = (z_j - lam) / v_j;
    //Rprintf("First option \n");
  }else if(z_j < 0 && lam < abs(z_j)){
    b_j = (z_j + lam) / v_j;
    //Rprintf("Second option \n");
  }else{
    b_j = 0;
    //Rprintf("Third option \n");
  }
  
  return b_j;

}

// [[Rcpp::export]]
arma::vec penalize_cpp(arma::vec y, arma::mat X, arma::vec B, double lam) {
  double eps = 1000000.0;
  double tol = 0.00001;
  unsigned int maxit = 500; 
  unsigned int iter = 0;
  unsigned int p = X.n_cols;
  unsigned int j;
  double b_j;
  arma::vec B_new(p);
  
  while(eps > tol && iter < maxit){ // while(((eps > tol) & (iter < maxit))){
  // Define B0 = B from last iteration - use in convergence calculation
    arma::vec B0 = B;
    
    for(j = 0; j < p; j++){
      
      if(iter == 0){ // Update all B[j]
        b_j = update(y, X, B, lam, j);
      }else{ // Only update non-zero B[j] after first round
        if(B(j) != 0){
          b_j = update(y, X, B, lam, j);
        }else{
          b_j = 0;
        }
      }
      
      B_new(j) = b_j;
    }
    
    // Evaluate convergence criteria - Euclidean distance
    eps = sqrt(as_scalar((B_new - B0).t()*(B_new - B0)));
      
    // Update iterations
      iter += 1;
      if(iter == maxit){warning("Model failed to converge within maximum number of iterations");}
      
  } 
    
// Return updated B (for each lambda)
    return B_new;
      
}



// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R

# # Simulate data to test Lasso function
# 
# # Number of samples
# n = 100
# 
# # Pick true Beta parameters
# beta = c(-2.5, 1, -2, 3, -4, numeric(10))
# p = length(beta)
# 
# covar = matrix(rnorm(100*(p-1)), nrow = n, ncol = p-1)
# 
# X = cbind(rep(1,n), covar)
# prob = exp(X%*%beta) / (1 + exp(X%*%beta))
# y = rbinom(n, 1, prob)
#   
# library(stats)
# fit.int = glm(y ~ 1, family = "binomial")
# int = fit.int$coefficients
# 
# # Result: intercept = -0.8473
# prob = exp(int) / (1 + exp(int))
# 
# # Find lambda_max and lambda_min
# W = diag(prob, nrow = n) # Weight matrix when B = 0
#   
# lam_option = numeric(ncol(X))
# for(j in 1:ncol(X)){
#   lam_option[j] = (1/nrow(X)) * abs(t(X[,j]) %*% W %*% (y - X[,-j] %*% B[-j]))
# }
# 
# lambda_max = max(lam_option)
#   epsilon = 0.001
# lambda_min = lambda_max * epsilon
#     
# # Recommendation by 761 notes: perform sequence on log scale
# log_lam = seq(from = log(lambda_max), to = log(lambda_min), by = -0.05)
#     
# ###################################################
#     
# # Testing Hillary's rough draft code
#     
# # In final results list, find B and lambda combo with the lowest BIC
#     
# # Run simulation in `Simulation.R` first
#   B = numeric(length = ncol(X))
#   results = list()
#   lambda = exp(log_lam)
#   results[[1]] = B
#   
#   bic.vec = numeric(length(lambda))
#   bic.vec[1] = 10^10
#     
#   for(l in 2:length(lambda)){
#     results[[l]] = penalize_cpp(y, X, B, lambda[l]) # Add "family = 'binomial'" 
#     B = results[[l]]
#     eta_vec = X %*% B
#     bic.vec[l] = -2 * sum( y * eta_vec - log(1 + exp(eta_vec)) ) / nrow(X) + 
#       sum(B!=0) * log(nrow(X))
#   }
#   
#   which.min(bic.vec)
#   results[[which.min(bic.vec)]]
*/

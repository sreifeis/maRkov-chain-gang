#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

double update(arma::vec y, arma::mat X, arma::vec B, double lam, unsigned int j){
  unsigned int n = X.n_rows;
  unsigned int p = X.n_cols;
  double b_j;
  arma::vec prob(n);
  unsigned int k = 0;
  arma::vec X_j = X.col(j);
  
  arma::vec eta = X * B;
  arma::vec mu = exp(eta) / (1 + exp(eta));
  arma::mat W = diagmat(mu);
  arma::vec res = W.i() * (y - mu) ;
  double v_j = (1 / n) * as_scalar(X_j.t() * W.i() * X_j);
  double z_j = (1 / n) * as_scalar(X_j.t() * W.i() * res);
  
  // Lasso solution: soft thresholding penalty
  
  return b_j;
}

NumericVector update(arma::vec y, arma::mat X, double lam, // Pass relevant penalization parameter value
                     arma::vec alpha, arma::vec coef, double mcp_gamma,
                     char family, unsigned int j){
  
  unsigned int i, k;
  unsigned int q = alpha.n_elem;
  unsigned int N = X.n_rows;
  // Need to generalize X_j to include all columns for group j (one or more)
  arma::mat X_j = X.col(j);
  arma::vec mu(N);
  arma::vec weight(N);
  arma::mat W(N,N);
  arma::vec coef_j(j); // Need to generalize for vector of coefficients
  
  
  arma::vec eta ; // eta, the linear predictor, will be the same for all families
  
  if(fam = "binomial"){
    mu = exp(eta) / (1 + exp(eta));
    weight = mu * (1 - mu);
    W = diagmat(weight);
    
  } else if(fam = "poisson"){
    mu = exp(eta);
    weight = mu;
    W = diagmat(weight);
  }
  // Extend to multiple glm families
  
  arma::vec resid = W.i() * (y - mu);
  arma::vec V_j = (1 / N) * X_j.t() * W * X_j;
  arma::vec Z_j = (1 / N) * X_j.t() * W * resid + V_j * coef.col(j); 
  // need to generalize coef.col(j) for vector of coefficients
  
  // Update according to MCP penalty solution
  double L2_norm = sqrt(as_scalar(Z_j.t() * Z_j));
  double st; // st = soft thresholding result
  double adj = (1 - 1 / mcp_gamma); // Adjusting constant to divide by in soft thresholding
  double soln;
  
  if(L2_norm <= (mcp_gamma * lam)){
    // Figure out how soft thresholding works when L2_norm always positive. Ignore 3rd case?
  } else if(L2_norm > (mcp_gamma * lam)){
    soln = L2_norm;
  }
  
  coef_j = soln * Z_j / L2_norm;
  return coef_j; // Why error here, and how to fix?
}

// [[Rcpp::export]]
List penalize(NumericVector x) {
  // return x * 2;
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R
# timesTwo(42)
*/

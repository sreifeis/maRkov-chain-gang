#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

double update(arma::vec y, arma::mat X, arma::vec B, double lam, unsigned int j){
  unsigned int n = X.n_rows;
  unsigned int p = X.n_cols;
  double b_j;
  arma::vec prob(n);
  unsigned int k = 0;
  
  arma::vec eta = X * B;
  for(k = 0; k < n; k++){
    prob[k] = exp(eta[k]) / (1 + exp(eta[k]));
    }
  arma::mat W = diagmat(prob);
  arma::vec res = inv(W) * (y - p) ;
  
  return b_j;
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

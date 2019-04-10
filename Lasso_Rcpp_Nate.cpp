#define RCPP_ARMADILLO_RETURN_COLVEC_AS_VECTOR
#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

// [[Rcpp::export]]
double soft_thresh( double z, double lambda ) {
  
  // # Soft-thresholding function - returns a scalar;
  // Input;
  // z: continuous value;
  // lambda: continuous value;
  
  arma::mat Z2(1,1);
  Z2 = z * z;
  double abs_z = as_scalar( sqrt(Z2) );
  
  double val;
  if( z > 0 && lambda < abs_z ){
    val = z - lambda;
  }
  if( z < 0 && lambda < abs_z ){
    val = z + lambda;
  }
  if( lambda >= abs_z ){
    val = 0;
  }
  
  return val;
  
}

// [[Rcpp::export]]
arma::vec LQA_lasso( arma::mat X, arma::vec Y, arma::vec beta_start, double lambda, double tol, int max_it ) {
  
  // # Lasso function - returns a vector with double-type entries;
  
  // Input;
  // X: matrix of covariates;
  // Y: response vector with binary values;
  // beta_start: initial values for beta;
  // lambda: pre-defined value for lambda;
  // tol: tolerance level;
  // max_it: maximum number of iterations;
  
  // ### General LQA Algorithm ###;
  // # Fix the value of lambda;
  // # Outer loop: update the quadratic approximation using the current parameters beta.tilde;
  // # Innter loop: run the coordinate descent algorithm on the penalized weighted least squares problem;
  // # Iterate between the two loops;
  
  // # Input: X, Y, beta_start, lambda, tol, max_it;
  int n = X.n_rows;
  int p = X.n_cols;
  arma::vec beta_tilde = beta_start;
  arma::vec beta = arma::ones(p);
  
  // # Set up vectors and values to use in loop;
  int iter = 0;
  arma::vec X_beta = arma::zeros(n);
  arma::vec ones_vec = arma::ones(n);
  arma::vec p_tilde_vals = arma::zeros(n);
  arma::vec z_vals = arma::zeros(n);
  arma::vec w_vals = arma::zeros(n);
  arma::mat X_withoutj(n,p);
  arma::vec beta_withoutj(p);
  arma::vec X_beta_withoutj(n);
  double st_input = 0.0;
  double beta_denom = 0.0;
  arma::mat sqr_diff(1,1);
  double abs_diff = 0.0;
  arma::vec big_abs_diff = arma::zeros(p);
  double abs_diff_sum = 1.0;
  
  // # Outer loop;
  while( abs_diff_sum != 0 && iter < max_it ) {
    
    // # Save current values of beta.tilde as beta and increase iteration count;
    beta = beta_tilde;
    iter = iter + 1;
    
    // # Inner loop - coordinate descent algorithm;
    for(int j = 0; j < p; j++) {
      
      // # Calculate p.tilde.i, z.i, w.i for i=1,...,n;
      X_beta = X * beta_tilde;
      p_tilde_vals = exp( X_beta ) / ( ones_vec + exp( X_beta ) );
      z_vals = X_beta + ( Y - p_tilde_vals ) / ( p_tilde_vals % ( ones_vec - p_tilde_vals ) );
      w_vals = p_tilde_vals % ( ones_vec - p_tilde_vals );
      
      // # Soft-threshold input and beta.tilde_j calculation;
      X_withoutj = X;
      beta_withoutj = beta_tilde;
      X_withoutj.shed_col(j);
      beta_withoutj.shed_row(j);
      X_beta_withoutj = X_withoutj * beta_withoutj;
      st_input = sum( w_vals % X.col(j) % ( z_vals - X_beta_withoutj ) ) / n;
      beta_denom = sum( w_vals % X.col(j) % X.col(j) ) / n;
      beta_tilde(j) = soft_thresh( st_input, lambda ) / beta_denom;
      
      //Rprintf( "p[0]: %f, z[0]: %f, w[0]: %f, st_input: %f, beta_denom: %f, beta_tilde(j): %f \n",;
               //p_tilde_vals(0), z_vals(0), w_vals(0), st_input, beta_denom, beta_tilde(j) );
      
    }
    
    // # Calculate the absolute value of the differences between current and previous betas;
    // # Count the number of beta differences between last and current iteration that are larger than tol;
    for(int k = 0; k < p; k++) {
      sqr_diff = ( (beta(k) - beta_tilde(k)) * (beta(k) - beta_tilde(k)) );
      abs_diff = as_scalar( sqrt( arma::pow(sqr_diff, 2) ) );
      if(abs_diff > tol){
        big_abs_diff(k) = 1;
      }
    }
    abs_diff_sum = sum(big_abs_diff);
    
  }
  
  return beta_tilde;
  
}
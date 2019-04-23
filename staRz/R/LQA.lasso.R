#' LQA Lasso
#'
#' This function performs LASSO regression given a design matrix, an outcome vector,
#' starting values for the regression parameters, and a penalty coefficient.
#' 
#' @param X design matrix with an intercept column and numeric variables that have been centered and scaled
#' @param Y vector of binary outcomes taking values 0 and 1
#' @param beta_start vector of starting values for the parameters, should have length equal to number of columns in X
#' @param lambda scalar value of the penalty coefficient
#' @param tol desired tolerance level for convergence criteria
#' @param max_it maximum number of iterations
#' 
#' @return a vector of parameter estimates beta_tilde, with length equal to the number of columns in X
#' 
#' @useDynLib staRz, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' 
#' @export
LQA.lasso <- function(X, Y, beta_start, lambda, tol, max_it){
  if (!all(Y %in% c(0,1))) {
    stop("Y must contain only 0's and 1's")
  }
  if (length(tol) > 1) {
    stop("tol must be a scalar")
  }
  if (length(max_it) > 1 | sign(max_it) < 1 | ceiling(max_it) != max_it | floor(max_it) != max_it) {
    stop("max_it must be a positive integer")
  }
  if (ncol(X) != length(beta_start)) {
    stop("Number of columns of X and length of beta_start must be equal")
  }
  if (any(sapply(1:ncol(X), function(i) any(class(X[,i]) == "character")))) {
    stop("X matrix cannot contain any character values")
  }
  LQA_lasso(X, Y, beta_start, lambda, tol, max_it)
}

#' LQA Lasso
#'
#' This function does...
#' 
#' @param X description
#' @param Y description
#' @param beta_start description
#' @param lambda description
#' @param tol description
#' @param max_it description
#' 
#' @return a vector of parameters beta_tilde
#' 
#' @examples 
#' give examples
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

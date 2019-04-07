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
#' @export
LQA.lasso <- function(X, Y, beta_start, lambda, tol, max_it){
  LQA_lasso(X, Y, beta_start, lambda, tol, max_it)
}

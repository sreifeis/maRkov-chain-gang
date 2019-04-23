#' Soft Threshold
#'
#' This function produces a soft threshold value to be used in the LASSO function 
#' to compute the updated vector of parameter estimates. 
#' 
#' @param z numeric value
#' @param lambda numeric value of the penalty coefficient
#' 
#' @return a scalar value
#' 
#'
#' @export
soft.thresh <- function(z, lambda){
  soft_thresh(z, lambda)
}
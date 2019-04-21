#' Sentiment analysis variables and review classifications, training data.
#'
#' A dataset containing the LDA variables and human-generated variables
#' used to help classify Amazon reviews and titles. 
#' 
#' @docType data
#'
#' @usage data(all.train)
#'
#' @format A data frame with 16699 rows and 33 variables:
#' \describe{
#'   \item{V1}{intercept column of X.train}
#'   \item{y.train}{binary classification of stars to less than or greater than 4}
#'   ...
#' }
#' @source \url{https://github.com/sreifeis/maRkov-chain-gang}
"all.train"
#' Design matrix containing sentiment analysis variables and Amazon review classifications (test data)
#'
#' A dataset containing the LDA variables and human-generated variables
#' used to help classify reviews and titles for Amazon products. The first 
#' 32 columns correspond to the design matrix and the last column is the 
#' vector of outcomes. If the dataset is split into its two component 
#' parts, X.test and y.test, it can be used along with a chosen value of
#' the penalty coefficient as inputs to the LQA.lasso function in this package. 
#' This dataset contains half of the total data from the original dataset, to
#' be used as the test dataset for LASSO regression. 
#' 
#' @docType data
#'
#' @format A matrix with 16700 rows and 33 variables:
#' \describe{
#'   \item{V1}{intercept column of X.train}
#'   \item{title_p_topic1}{posterior probability of the title topic 1 (from LDA)}
#'   ...
#'   \item{title_p_topic5}{posterior probability of the title topic 5 (from LDA)}
#'   \item{review_p_topic1}{posterior probability of the review topic 1 (from LDA)}
#'   ...
#'   \item{review_p_topic20}{posterior probability of the review topic 20 (from LDA)}
#'   \item{text_word_ct}{total word count in the review}
#'   \item{text_pos_prop}{proportion of positive words in the review}
#'   \item{text_neg_prop}{proportion of negative words in the review}
#'   \item{title_word_ct}{total word count in the title}
#'   \item{title_pos_prop}{proportion of positive words in the title}
#'   \item{title_neg_prop}{proportion of negative words in the title}
#'   \item{y.train}{binary classification of stars to less than or greater than 4}
#' }
#' @source \url{https://github.com/sreifeis/maRkov-chain-gang}
"all.test"
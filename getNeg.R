#' Get Negative Words
#'
#' Gets the negative words based on the bing definition in
#' get_sentiments()
#' 
#' Creates a data frame of the bing words and their corresponding 
#' sentiment which can be either positive or negative. Then it
#' creates a vector of only the negative words.
#' 
#' @return returns a vector of the negative words
#' 
#' @importFrom tidytext get_sentiments
#' 
#' @export
getNeg <- function()
{
  bing_df<- tidytext::get_sentiments("bing")
  neg_words <- bing_df$word[which(bing_df$sentiment == "negative")]
  return(neg_words)
}

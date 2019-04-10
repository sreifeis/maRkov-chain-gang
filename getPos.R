#' Get Positive Words
#'
#' Gets the positive words based on the bing definition in
#' get_sentiments()
#' 
#' Creates a data frame of the bing words and their corresponding 
#' sentiment which can be either positive or negative. Then it
#' creates a vector of only the positive words.
#' 
#' @return returns a vector of the positive words
#' 
#' @importFrom tidytext get_sentiments
#' 
#' @export
getPos <- function()
{
  bing_df<- tidytext::get_sentiments("bing")
  pos_words <- bing_df$word[which(bing_df$sentiment == "positive")]
  
  if(length(pos_words)==0)
  {
    stop("There are no positive words")
  }
  
  return(pos_words)
}
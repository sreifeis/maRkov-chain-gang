#' Get Count Data
#'
#' Gets the number of words, number/proportion of positive words,
#' and number/proportion of negative words for each sentence in the
#' sentence vector.
#'
#' First, we clean each sentence in the vector, then we get the word count,
#' and then we get the number/proportion of positive words,
#' and number/proportion of negative words for each sentence in the
#' sentence vector.
#'
#' This function is an envelope function for cleanSentence(), wordcount(), getPos(),
#' getNeg(), and getSentCount(). This function just makes it easier to use everything
#' in one fell swoop for the purposes of our project. You can use each of the functions
#' independently if you would rather - especially if you would like to supply your own
#' vector of positive and negative words.
#' 
#' @param sentence_vector a vector of sentences - factor or character - that
#' we wish to analyze.
#' @return a data frame with columns: wd_ct, pos_ct, pos_prop, neg_ct, neg_prop. Which
#' are the number of words, number of positive words, proprtion of positive words, 
#' number of negative words and proportion of negative words in the sentence, respectively.
#' 
#' @importFrom ngram wordcount
#' 
#' @export 
getCountData <- function(sentence_vector)
{
  if(length(sentence_vector)==1)
  {
    if(sentence_vector== "" | is.na(sentence_vector)){
      stop("You provided an empty vector")
    }
  }
  
  sentence_clean <- sapply(sentence_vector, function(x) cleanSentence(x))
  sentence_wd_ct <- unlist(lapply(sentence_clean, function(x) ngram::wordcount(x)))
  
  pos_words <- getPos()
  neg_words <- getNeg()
  
  len_clean <- length(sentence_clean)
  
  sent_mat <- t(sapply(seq(1,len_clean), function(x) getSentCount(sentence_clean[x],sentence_wd_ct[x],pos_words, neg_words)))
  sent_df <- as.data.frame(cbind(sentence_wd_ct,sent_mat))
  colnames(sent_df) <- c("wd_ct","pos_ct","pos_prop","neg_ct","neg_prop")
  return(sent_df)
}
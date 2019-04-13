#' Get Sentiment Count
#'
#' For a given, clean, sentence, it gets the number of positive 
#'  and number of negative words in the sentence.
#'
#' First, if the word count is greater than zero, we create a
#' vector of words in the sentence. Then we count the number of
#' words in the sentence that are in the positive vector and the 
#' number that are in the negative vector. We also calculate the
#' proportion of positive words and proportion of negative words.
#' 
#' The default vector of positive and negative words are from the
#' bing definition in the get_sentiments() function. However, 
#' users are welcome to supply their own vector of words if they
#' prefer.
#' 
#' If the word count is zero, it returns all missing values
#'
#' 
#' @param clean_sentence a single sentence that has been standardized
#' @param wdct the number of words in the sentence. Default is to calculate
#' word count if it wasn't supplied
#' @param pos_words a vector of words that are considered positive. Default
#' is to use the bing definition in the get_sentiments() function.
#' @param neg_words a vector of words that are considered negative. Default
#' is to use the bing definition in the get_sentiments() function.
#' @return a vector with number of positive words, proportion of positive 
#' words,number of negative words, and the proportion of negative words. 
#' 
#' @importFrom stringr word
#' @importFrom ngram wordcount
#' 
#' @export 
getSentCount <- function(clean_sentence,wdct=ngram::wordcount(clean_sentence),pos_words=getPos(),neg_words=getNeg())
{
  if(clean_sentence != cleanSentence(clean_sentence))
  {
    stop("You must run cleanSentence() on the sentence first")
  }
  
  if(wdct != ngram::wordcount(clean_sentence))
  {
    stop("Word count you supplied does not match the actual word count of your sentence")
  }
  
  if(length(pos_words)==1){
    if(pos_words==""){
      warning("You did not provide any positive words")
    }
  }  
  
  if(length(neg_words)==1){
    if(neg_words==""){
      warning("You did not provide any negative words")
    }
  }  
  
  #If word count is 0, then there are cannot be any positive
  # or negative words and thus they should return NA values
  if(wdct == 0){
    
    warning("Wordcount is 0 for this sentence")
    
    pos_ct <- NA
    pos_prop <- NA
    
    neg_ct <- NA
    neg_prop <- NA
  }else{
    #creates a vector of the words in the sentence
    word_vector <- stringr::word(clean_sentence,1:wdct)
    
    #counts the positive words in the sentence
    # and calculates the proportion
    pos_ct <- sum(word_vector %in% pos_words)
    pos_prop <- pos_ct/wdct
    
    #counts the positive words in the sentence
    # and calculates the proportion
    neg_ct <- sum(word_vector %in% neg_words)
    neg_prop <- neg_ct/wdct
  }
  return(c(pos_ct,pos_prop,neg_ct,neg_prop))
}
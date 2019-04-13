#' Clean Sentence
#'
#' Standardizes the sentence so that it is in a proper format to be 
#' analyzed in other functions.
#'
#' First, we convert the sentence to character in case it is of
#' variable type factor. Then, we make the sentence lower case,
#' and finally we remove all punctuation from the sentence.
#'
#' 
#' @param sentence a single sentence - character or factor
#' @return the cleaned sentence
#' 
#' @importFrom tm removePunctuation
#' 
#' @export 
cleanSentence <- function(sentence)
{
  if(length(sentence) > 1)
  {
    stop("Sentence should just be one character (or factor) variable, not a vector")
  }
  
  if(typeof(sentence) == "list" & lengths(sentence) > 1)
  {
    stop("Length of the (only) element in the list must be 1!")
  }
  
  #converts to character
  char_sent <- as.character(sentence)
  
  #converts all words to lower case
  low_case <- tolower(char_sent)
  
  #removes all punctuation in the sentence
  clean_sentence <- tm::removePunctuation(low_case)
  
  return(clean_sentence)
}
% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cleanSentence.R
\name{cleanSentence}
\alias{cleanSentence}
\title{Clean Sentence}
\usage{
cleanSentence(sentence)
}
\arguments{
\item{sentence}{a single sentence - character or factor}
}
\value{
the cleaned sentence
}
\description{
Standardizes the sentence so that it is in a proper format to be 
analyzed in other functions.
}
\details{
First, we convert the sentence to character in case it is of
variable type factor. Then, we make the sentence lower case,
and finally we remove all punctuation from the sentence.
}

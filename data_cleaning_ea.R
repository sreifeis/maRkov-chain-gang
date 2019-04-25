remove(list = ls())


############################################################################
## Programmer: Ethan Alt
## Last updated: 03/28/2019
## 
## Description of file:
## 
## This file inputs the data into R. Performs some basic cleaning, and runs
## latent dirichlet allocation (LDA) on the data set. 
############################################################################




##################################################################
## Put all packages to be loaded in pkglist. Script will check if 
## package is installed. If not, it will install and then load.
##################################################################
pkglist <- c("data.table", "text2vec", "magrittr", "topicmodels", "tm", "Matrix")



## Function to check if package is installed. Will install
## the package if it is not, and then load

install.pkg <- function(pkgname, character.only = TRUE) {
  if(!(pkgname %in% rownames(installed.packages()))) {
    install.packages(pkgname)
  }
  require(pkgname, character.only = character.only)
}

## This applies the install.pkg function over the length
## of the pkglist vector
lapply(pkglist, install.pkg, character.only = TRUE)




###############################################
## Load the data from CSV
###############################################
setwd("C:/Users/ethanalt/Google Drive/BIOS 735/Data")
amzn.df <- fread("1429_1.csv")



######################################################
## Subset the data to only include the review message
######################################################

id <- amzn.df$id
review <- amzn.df$reviews.text
title <- amzn.df$reviews.title


#######################################
## Get corpus of reviews and titles
#######################################
source.review <- VectorSource(review)
corpus.review <- Corpus(source.review)

source.title <- VectorSource(title)
corpus.title <- Corpus(source.title)



###################################
## Put corpi in list for looping
###################################
corpus.list <- list(corpus.review, corpus.title)
n.topics <- c(20, 5)
type = c("review", "title")


#################################################
## Loop through corpi, outputting
## the topics and the posterior
## probabilities as CSV files. 
#################################################

terms.list <- list()

for(c in 1:2) {
  corpus = corpus.list[[c]]
  
  
  ## Remove stopwords (common words that don't convey sentiment e.g., "i", "me", "they")
  exclude <- c("amazon", "alexa", "echo", "tablet", "kindle", "fire", "alexia", 
               "read", "reading", "ipad", "husband", "son", "daughter", "kids", "kid")
  stopwords <- c(stopwords("en"), exclude)
  corpus <- tm_map(corpus, removeWords, stopwords)
  
  ## remove whitespace
  corpus <- tm_map(corpus, stripWhitespace)
  ## convert to lowercase
  corpus <- tm_map(corpus, content_transformer(tolower))
  
  ## Remove stopwords (common words that don't convey sentiment e.g., "i", "me", "they")
  exclude <- c("amazon", "alexa", "echo", "tablet", "kindle", "fire", "alexia", 
               "read", "reading", "ipad")
  stopwords <- c(stopwords("en"), exclude)
  corpus <- tm_map(corpus, removeWords, stopwords)
  
  ## remove punctuation
  corpus <- tm_map(corpus, removePunctuation)
  ## Strip digits
  corpus <- tm_map(corpus, removeNumbers)
  
  
  ## Crate a document term matrix
  dtm <- DocumentTermMatrix(corpus)
  
  
  ## Cleaning up the reviews resulted in some of
  ## the reviews being empty. Remove these
  dtm <- dtm[unique(dtm$i), ]
  id2 <- id[unique(dtm$i)]
  
  #########################################
  ## Latent Dirichlet Allocation (LDA)
  #########################################
  
  ## Set parameters for Gibbs sampling
  burnin <- 4000
  iter <- 2000
  thin <- 500
  seed <- list(2003, 5, 63, 100001, 765)
  nstart <- 5
  best <- TRUE
  
  ## Number of topics
  k <- n.topics[c]
  
  
  ## Run LDA using Variational Bayes EM algorithm
  ldaOut <- topicmodels::LDA(dtm,
                             k, 
                             method = "VEM"
  )
  
  
  ## write out results
  ## docs to topics
  ldaOut.topics <- as.matrix(topics(ldaOut))
  
  
  # top 50 terms in each topic
  terms.list[[c]] <- as.matrix(terms(ldaOut,50))
  
  
  # probabilities associated with each topic assignment
  topicProbabilities <- as.data.frame(ldaOut@gamma)
  names(topicProbabilities) <- paste0("p_topic", 1:ncol(topicProbabilities))

  
  ## Output latent topic and posterior probabilities
  setwd("C:/Users/ethanalt/Google Drive/BIOS 735/Data")
  filename <- paste0(type[c], "_topics.csv")
  df = data.frame(id = id2, topic = ldaOut.topics, topicProbabilities)
  names(df)[-1] <- paste0(type[c], "_", names(df)[-1])
  write.csv(df, file = filename, row.names = FALSE)
}

# 
# ## remove whitespace
# corpus <- tm_map(corpus, stripWhitespace)
# ## convert to lowercase
# corpus <- tm_map(corpus, content_transformer(tolower))
# 
# ## Remove stopwords (common words that don't convey sentiment e.g., "i", "me", "they")
# exclude <- c("amazon", "alexa", "echo", "tablet", "kindle", "fire", "alexia", 
#              "read", "reading", "ipad")
# stopwords <- c(stopwords("en"), exclude)
# corpus <- tm_map(corpus, removeWords, stopwords)
# 
# ## remove punctuation
# corpus <- tm_map(corpus, removePunctuation)
# ## Strip digits
# corpus <- tm_map(corpus, removeNumbers)
# 
# 
# 
# ## Crate a document term matrix
# dtm <- DocumentTermMatrix(corpus)
# 
# 
# ## Cleaning up the reviews resulted in some of
# ## the reviews being empty. Remove these
# dtm <- dtm[unique(dtm$i), ]
# 
# #########################################
# ## Latent Dirichlet Allocation (LDA)
# #########################################
# 
# ## Set parameters for Gibbs sampling
# burnin <- 4000
# iter <- 2000
# thin <- 500
# seed <- list(2003, 5, 63, 100001, 765)
# nstart <- 5
# best <- TRUE
# 
# ## Number of topics
# k <- 50
# 
# 
# ## Run LDA using Variational Bayes EM algorithm
# ldaOut <- topicmodels::LDA(dtm,
#               k, 
#               method = "VEM"
# )
# 
# 
# ## write out results
# ## docs to topics
# ldaOut.topics <- as.matrix(topics(ldaOut))
# 
# 
# 
# # top 6 terms in each topic
# ldaOut.terms <- as.matrix(terms(ldaOut,50))
# 
# 
# # probabilities associated with each topic assignment
# topicProbabilities <- as.data.frame(ldaOut@gamma)


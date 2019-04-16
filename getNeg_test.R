context("getNeg")

test_that("error for bad input",{
  expect_error(getNeg(c("sucky","terrible","worse")))
})

bing <- tidytext::get_sentiments("bing")
negative<- bing$word[which(bing$sentiment == "negative")]

test_that("equality",{
  expect_equal(getNeg(),negative)
})
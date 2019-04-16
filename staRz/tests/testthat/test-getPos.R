context("getPos")

test_that("error for bad input",{
  expect_error(getPos(c("great","wonderful")))
})

bing <- tidytext::get_sentiments("bing")
positive<- bing$word[which(bing$sentiment == "positive")]

test_that("equality",{
  expect_equal(getPos(),positive)
})
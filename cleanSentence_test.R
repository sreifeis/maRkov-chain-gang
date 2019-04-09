context("cleanSentence")

test_that("error for bad input",{
  expect_error(cleanSentence())
  expect_error(c("first sentence","second sentence"))
  expect_error(list(c("first sentence","second sentence"),"second element"))
  expect_error(list(c("first sentence","second sentence")))
})

test_that("equality",{
  expect_equal(cleanSentence("This IS a sEntence. aND THIs!"),"this is a sentence and this")
  expect_equal(cleanSentence("...this i.s ME?Ssed uP.,"),"this is messed up")
})
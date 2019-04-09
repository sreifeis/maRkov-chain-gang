context("getCountData")

test_that("error for bad input",{
  expect_error(getCountData())
  expect_error(getCountData(""))
  expect_error(getCountData(NA))
})

#sentences i want to test
s1 <- "i LOVE this produc!t"
s2 <- "I can't WAIT to read my favorite books on this"
s3 <- "THis is MY first? reVIEw... I haTE reaDing.!"
s4 <- "the Best BOOK! It is fantAstic I DONOT hate it."

sentences <- data.frame(sentence = rbind(s1,s2,s3,s4),row.names = NULL)

#the data frame that should results from getCountData(sentence$sentences)
true_df <- as.data.frame(rbind(c(4,1,0.25,0,0),
                               c(10,1,0.1,0,0),
                               c(8,0,0,1,0.125),
                               c(10,2,0.2,1,0.1)))
colnames(true_df) <- c("wd_ct","pos_ct","pos_prop","neg_ct","neg_prop")

test_that("equality",{
  expect_equal(getCountData(sentences$sentence),true_df)
})
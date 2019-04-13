context("getSentCount")

test_that("error for bad input",{
  expect_error(getSentCount())
  expect_error(getSentCount("great job",1))
  expect_error(getSentCount("Not CLEAN!",2))
})

#These inputs could be correct, we just want to warn the user
# in case they input the parameters incorrectly
test_that("warning for iffy input",{
  expect_warning(getSentCount("",0))
  expect_warning(getSentCount("this is great",3,"","awful"))
  expect_warning(getSentCount("this is great",3,"great",""))
  expect_warning(getSentCount("this is great",3,"",""))
})

test_that("equality",{
  #great is positive
  expect_equal(getSentCount("great job"),c(1,0.5,0,0))
  
  #terrible is negative
  expect_equal(getSentCount("terrible job"),c(0,0,1,0.5))
  
  #works and great are positive
  #slow is negative
  expect_equal(getSentCount("works great but runs slow"),c(2,0.4,1,0.2))
})

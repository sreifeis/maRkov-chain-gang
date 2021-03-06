context("dichotomizeStarz")

test_that("error for bad input",{
  expect_error(dichotomizeStarz())
  expect_error(dichotomizeStarz(c(1, 2, NA)))
  expect_error(dichotomizeStarz(c(1,2,7)))
  expect_error(dichotomizeStarz(seq(0,5)))
  expect_error(dichotomizeStarz(c(1.5,3.2,4.7)))
})

test_that("equality",{
  expect_equal(dichotomizeStarz(seq(1,5)),c(0,0,0,1,1))
})
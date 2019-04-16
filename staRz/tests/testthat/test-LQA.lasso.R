context("LQA.lasso")
test_that("simple errors for bad input", {
  expect_error(LQA.lasso()) #missing a required input
  expect_error(LQA.lasso(X=matrix("a", nrow=2, ncol=3), 
                         Y=rbinom(2, 1, 0.5), beta_start=rnorm(3), 
                         lambda=1, tol=10^(-3), max_it=100)) #characters in X
  expect_error(LQA.lasso(X=matrix(1, nrow=2, ncol=3), 
                         Y=rbinom(2, 1, 0.5), beta_start=rnorm(7), 
                         lambda=1, tol=10^(-3), max_it=100)) #beta_start length neq ncol in X
  expect_error(LQA.lasso(X=matrix(1, nrow=2, ncol=3), 
                         Y=rbinom(2, 1, 0.5), beta_start=rnorm(3), 
                         lambda=1, tol=c(0.1, 0.01), max_it=100)) # tol not scalar
  expect_error(LQA.lasso(X=matrix(1, nrow=2, ncol=3), 
                         Y=rbinom(2, 1, 0.5), beta_start=rnorm(3), 
                         lambda=1, tol=10^(-3), max_it=-100)) #max_it not positive integer
  expect_error(LQA.lasso(X=matrix(1, nrow=2, ncol=3), 
                         Y=rnorm(2, 1, 5), beta_start=rnorm(3), 
                         lambda=1, tol=10^(-3), max_it=100)) #Y contains non-{0,1} elements
})

# test_that("warnings for bad output", {
#   expect_warning(LQA.lasso()) # inputs that yield NA values in output
# })
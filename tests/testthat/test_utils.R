library(testthat)

# Test numeric input check success
test_that("Test numeric input check success", {
  arg <- 4
  expect_no_error(check_arg_num(arg))
})

# Test numeric input check failure
test_that("Test numeric input check failure", {
  arg1 <- -1
  arg2 <- "4"
  expect_error(check_arg_num(arg1))
  expect_error(check_arg_num(arg2))
})

# Test numeric range input check success
test_that("Test numeric range input check success", {
  arg <- 0.5
  arg_min <- 0
  arg_max <- 1
  expect_no_error(check_arg_numrange(arg, arg_min, arg_max))
})

# Test range input check failure
test_that("Test numeric range input check failure", {
  arg1 <- -1
  arg2 <- "0.5"
  arg_min <- 0
  arg_max <- 1
  expect_error(check_arg_numrange(arg1, arg_min, arg_max))
  expect_error(check_arg_numrange(arg2, arg_min, arg_max))
})

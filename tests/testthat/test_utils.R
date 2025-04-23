library(testthat)

# Test whether argument name is returned on input error
test_that("Test whether argument name is returned on input error", {
  arg <- "10"
  arg_name <- "flux"
  expect_error(check_arg_num(arg, arg_name), "Error: parameter flux must be a positive number.")
})


# Test numeric input check success
test_that("Test numeric input check success", {
  arg <- 4
  arg_name <- "valid_arg"
  expect_no_error(check_arg_num(arg, arg_name))
})


# Test numeric input check failure
test_that("Test numeric input check failure", {
  arg1 <- -1
  arg2 <- "4"
  arg_name <- "invalid_arg"
  expect_error(check_arg_num(arg1, arg_name))
  expect_error(check_arg_num(arg2, arg_name))
})


# Test numeric range input check success
test_that("Test numeric range input check success", {
  arg <- 0.5
  arg_min <- 0
  arg_max <- 1
  arg_name <- "valid_arg"
  expect_no_error(check_arg_numrange(arg, arg_min, arg_max, arg_name))
})


# Test range input check failure
test_that("Test numeric range input check failure", {
  arg1 <- -1
  arg2 <- "0.5"
  arg_min <- 0
  arg_max <- 1
  arg_name <- "invalid_arg"
  expect_error(check_arg_numrange(arg1, arg_min, arg_max))
  expect_error(check_arg_numrange(arg2, arg_min, arg_max))
})

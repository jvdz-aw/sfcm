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


# Test numeric column input check success
test_that("Test numeric column input check success", {
  col_vals <- c(10,15,20)
  col_name <- "valid_numeric_col"
  expect_no_error(check_col_num(col_vals, col_name))
})


# Test numeric column input check failure
test_that("Test numeric column input check failure", {
  col_vals_negative <- c(10, -10, -5)
  col_vals_string <- c("10", "15", "20")
  col_name <- "invalid_numeric_col"
  expect_error(check_col_num(col_vals_negative, col_name))
  expect_error(check_col_num(col_vals_string, col_name))
})


# Test numeric column range input check success
test_that("Test numeric column range input check success", {
  col_values <- c(0.5, 0.1, 0.3)
  col_min <- 0
  col_max <- 1
  col_name <- "valid_numrange_col"
  expect_no_error(check_col_numrange(col_values, col_min, col_max, col_name))
})


# Test numeric column range input check failure
test_that("Test numeric range input check failure", {
  col_vals_negative <- c(-0.2, 10, 0.5)
  col_vals_string <- c("0.5", "0.1", "0.3")
  col_min <- 0
  col_max <- 1
  col_name <- "invalid_numrange_col"
  expect_error(check_col_numrange(col_vals_negative, col_min, col_max))
  expect_error(check_col_numrange(col_vals_string, col_min, col_max))
})

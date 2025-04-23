library(testthat)

# Define global test parameters
n_iter <- 1000
par_name <- "flux"
set.seed(1337) # For reproducibility


# Test draw parameter success using PointEstimate method
test_that("PointEstimate parameter draw success", {

  # Define test parameters
  point_estimate <- 4

  # Run test
  result <- draw_parameters(par_name = par_name,
                            draw_method = "PointEstimate",
                            par_vec = point_estimate,
                            n_iterations = n_iter)

  # Check data types of columns
  expect_s3_class(result[[1]], "factor")
  expect_type(result[[2]], "double")

  # Should produce a data frame with two columns and 100 rows
  expect_equal(ncol(result), 2)
  expect_equal(nrow(result), n_iter)

  # Second column only contains the value given by `point_est`
  expect_length(unique(result[,2]), length(point_estimate)) # Number of unique point estimates (should be 1)
  expect_length(result[,2], n_iter) # Number of point estimates (should be equal to number of iterations)


})


# Test draw parameter failure using PointEstimate method
test_that("PointEstimate parameter draw failure", {

  # Define test parameters
  point_estimate <- c(4, 5)

  # Run test
  expect_error(draw_parameters(par_name = par_name,
                               draw_method = "PointEstimate",
                               par_vec = point_estimate,
                               n_iterations = n_iter),
               regexp = "Error: `PointEstimate` requires a single parameter value.")
})


# Test draw parameter success using Bootstrap method
test_that("Bootstrap parameter draw success", {

  # Define test parameters
  par_vec <- c(4, 4, 5, 5, 8, 8, 10)
  n_iter <- 10000 # Set large number for testing bootstrapping proportions

  # Run test
  result <- draw_parameters(par_name = par_name,
                            draw_method = "Bootstrap",
                            par_vec = par_vec,
                            n_iterations = n_iter)

  # Check data types
  expect_s3_class(result[[1]], "factor")
  expect_type(result[[2]], "double")

  # Check length
  expect_length(result[[2]], n_iter)

  # Compare unique values
  exp_uniq <- sort(unique(par_vec))
  obs_uniq <- sort(unique(result[[2]]))
  expect_equal(obs_uniq, exp_uniq)

  # Compare proportions
  exp_props <- as.numeric(table(par_vec) / length(par_vec))
  obs_props <- as.numeric(table(result[[2]]) / n_iter)
  expect_equal(obs_props, exp_props, tolerance = 0.01)
})


# Test draw parameter failure using Bootstrap method
test_that("Bootstrap parameter draw failure", {

  # Define test parameters
  par_vec <- 4
  n_iter <- 10000 # Set large number for testing bootstrapping proportions

  # Run test
  expect_error(draw_parameters(par_name = par_name,
                               draw_method = "Bootstrap",
                               par_vec = par_vec,
                               n_iterations = n_iter),
               regexp = "Error: `Bootstrap` requires a vector of parameter values.")
})


# Test draw parameter success using TruncNorm method
test_that("TruncNorm parameter draw success", {

  # Define test parameters
  n_iter <- 10000 # Set large number for testing bootstrapping proportions
  mean_val <- 10
  sd_val <- 2
  lower_bound <- 5
  upper_bound <- 15

  # Run test
  result <- draw_parameters(par_name = par_name,
                            draw_method = "TruncNorm",
                            n_iterations = n_iter,
                            mean = mean_val,
                            sd = sd_val,
                            lower_bound = lower_bound,
                            upper_bound = upper_bound)
  # Check data types
  expect_s3_class(result[[1]], "factor")
  expect_type(result[[2]], "double")

  # Compare mean values
  exp_mean <- mean_val
  obs_mean <- mean(result[[2]])
  expect_equal(obs_mean, exp_mean, tolerance = 0.1)

  # Compare sd values
  exp_sd <- sd_val
  obs_sd <- sd(result[[2]])
  expect_equal(obs_sd, exp_sd, tolerance = 0.1)

  # Check bounds
  expect_true(all(result[[2]] >= lower_bound & result[[2]] <= upper_bound))
})


# Test draw parameter failure using TruncNorm method
test_that("TruncNorm parameter draw failure", {

  # Define test parameters
  n_iter <- 10000 # Set large number for testing bootstrapping proportions
  mean_val <- NULL
  sd_val <- 2
  lower_bound <- 5
  upper_bound <- 15

  # Run test
  expect_error(draw_parameters(par_name = par_name,
                               draw_method = "TruncNorm",
                               n_iterations = n_iter,
                               mean = mean_val,
                               sd = sd_val,
                               lower_bound = lower_bound,
                               upper_bound = upper_bound),
               regexp = ("Error: `TruncNorm` requires values for mean and SD."))
})


# Test draw parameter failure due to invalid draw method
test_that("Draw parameter failure due to invalid draw method", {

  # Define test parameters
  point_estimate <- 4
  bad_draw_method <- "pointestimate"

  # Run test
  expect_error(draw_parameters(par_name = par_name,
                               draw_method = bad_draw_method,
                               par_vec = point_estimate,
                               n_iterations = n_iter),
               regexp = "Error: invalid method specified. Valid options are `PointEstimate`, `Bootstrap` or `TruncNorm`.")
})

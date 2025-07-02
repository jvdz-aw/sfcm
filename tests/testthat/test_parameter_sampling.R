library(testthat)

# Set seed to ensure reproducibility
set.seed(1337)

test_that("sample_norm returns values with correct mean and sd", {

  # Generate test dataframe with two rows
  test_df <- data.frame(
    flux_mean = c(50, 100),
    flux_sd = c(10, 50)
  )

  # Simulate flux using normal distribution
  samples <- sample_norm(test_df, "flux", n = 1000)

  # Test: length of list should be equal to number of rows in dataframe
  obs_length <- length(samples)
  exp_length <- nrow(test_df)
  expect_equal(obs_length, exp_length)

  # Compute observed values
  obs_means <- purrr::map_dbl(samples, mean)
  obs_sds <- purrr::map_dbl(samples, sd)

  # Get expected values
  exp_means <- test_df$flux_mean
  exp_sds <- test_df$flux_sd

  # Test: means and sds should be close within tolerance
  expect_equal(obs_means, exp_means, tolerance = 0.2)
  expect_equal(obs_sds, exp_sds, tolerance = 0.2)

})


test_that("sample_poisson returns values with correct mean", {

  # Generate test dataframe with three rows
  test_df <- data.frame(
    flux_mean = c(50, 100, 150)
  )

  # Simulate flux using poisson distribution
  samples <- sample_poisson(test_df, "flux", n = 1000)

  # Test: length of list should be equal to number of rows in dataframe
  obs_length <- length(samples)
  exp_length <- nrow(test_df)
  expect_equal(obs_length, exp_length)

  # Compute observed values
  obs_means <- purrr::map_dbl(samples, mean)
  obs_sds <- purrr::map_dbl(samples, sd)

  # Get expected values: variance should equal the mean for poisson data
  exp_means <- test_df$flux_mean
  exp_sds <- sqrt(test_df$flux_mean) # Square root of mean should be equal to sd

  # Test: means and sds should be close within tolerance
  expect_equal(obs_means, exp_means, tolerance = 0.2)
  expect_equal(obs_sds, exp_sds, tolerance = 0.2)

})


test_that("sample_beta returns values with correct mean and sd", {

  # Generate test dataframe with two rows
  test_df <- data.frame(
    p_col_mean = c(0.5, 0.8),
    p_col_sd = c(0.05, 0.1)
  )

  # Simulate flux using beta distribution
  samples <- sample_beta(test_df, "p_col", n = 1000)

  # Test: length of list should be equal to number of rows in dataframe
  obs_length <- length(samples)
  exp_length <- nrow(test_df)
  expect_equal(obs_length, exp_length)

  # Compute observed values
  obs_means <- purrr::map_dbl(samples, mean)
  obs_sds <- purrr::map_dbl(samples, sd)

  # Get expected values: variance should equal the mean for poisson data
  exp_means <- test_df$p_col_mean
  exp_sds <- test_df$p_col_sd

  # Test: means and sds should be close within tolerance
  expect_equal(obs_means, exp_means, tolerance = 0.2)
  expect_equal(obs_sds, exp_sds, tolerance = 0.2)

})


test_that("sample_nbinom returns values with correct mean and sd", {

  # Generate test dataframe with two rows
  test_df <- data.frame(
    flux_mean = c(50, 100),
    flux_sd = c(10, 50)
  )

  # Simulate flux using normal distribution
  samples <- sample_nbinom(test_df, "flux", n = 1000)

  # Test: length of list should be equal to number of rows in dataframe
  obs_length <- length(samples)
  exp_length <- nrow(test_df)
  expect_equal(obs_length, exp_length)

  # Compute observed values
  obs_means <- purrr::map_dbl(samples, mean)
  obs_sds <- purrr::map_dbl(samples, sd)

  # Get expected values: variance should equal the mean for poisson data
  exp_means <- test_df$flux_mean
  exp_sds <- test_df$flux_sd

  # Test: means and sds should be close within tolerance
  expect_equal(obs_means, exp_means, tolerance = 0.2)
  expect_equal(obs_sds, exp_sds, tolerance = 0.2)
})

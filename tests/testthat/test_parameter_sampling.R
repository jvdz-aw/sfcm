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


test_that("get_sampling_dispatch returns the correct mappings", {

  # Get sampling dispatch
  dispatch <- get_sampling_dispatch()

  # Should be a list
  expect_type(dispatch, "list")

  # Check expected names
  exp_names <- c("normal", "poisson", "beta", "nbinom")
  expect_setequal(names(dispatch), exp_names)

  # All elements should be functions
  expect_true(all(purrr::map_lgl(dispatch, is.function)))

})


test_that("get_allowed_dists returns the correct mappings", {

  # Get allowed distributions
  allowed <- get_allowed_dists()

  # Should be a list
  expect_type(allowed, "list")

  # Check expected names
  exp_names <- c("flux", "turbs_e", "p_col")

  # Check element lengths
  expect_equal(length(allowed[[exp_names[1]]]), 3)
  expect_equal(length(allowed[[exp_names[2]]]), 1)
  expect_equal(length(allowed[[exp_names[3]]]), 1)

})


test_that("simulate_parameters runs without errors", {

  # Generate test dataframe with two rows
  test_df <- data.frame(
    species = c("A", "B"),
    flux_mean = c(50, 100),
    flux_sd = c(10, 50),
    a_macro = 0.95,
    f_prop = 1,
    h_prop = 0.46961326,
    h_prop_ref = 0.67,
    rotor_d = 170,
    rotor_d_ref = 60,
    turb_dist = 628.6667,
    turb_dist_ref = 250,
    turbs_e_mean = c(4, 2),
    turbs_e_sd = 0,
    turbs_e_ref = 4.242641,
    p_col_mean = c(0.5, 0.8),
    p_col_sd = c(0.05, 0.1)
  )

  parameters <- c("flux", "turbs_e", "p_col")
  n_sims <- 500

  # Should run without errors
  res <- expect_no_error(simulate_parameters(data = test_df,
                             parameters = parameters,
                             distributions = list("flux" = "poisson",
                                                  "turbs_e" = "poisson",
                                                  "p_col" = "beta"),
                             n = n_sims))

  # Check whether the correct columns are returned
  exp_cols <- c(names(dplyr::select(test_df, !tidyr::matches("_mean|_sd"))), "simulation_id", parameters)
  expect_setequal(exp_cols, names(res))

  # Number of rows should be equal to nrows test_df times n_sims
  exp_nrows <- nrow(test_df) * n_sims
  expect_equal(exp_nrows, nrow(res))
})

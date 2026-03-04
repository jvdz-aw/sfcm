library(testthat)

set.seed(1337)

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

test_that("validate_sample_method_input handles 'n' correctly", {
# Valid n should not throw error
  expect_silent(validate_sample_method_input(n = 10))
  expect_silent(validate_sample_method_input(n = 0))
  
  # Invalid n should throw errors
  expect_error(validate_sample_method_input(n = -1), "'n' must be a single non-negative integer")
  expect_error(validate_sample_method_input(n = 10.5), "'n' must be a single non-negative integer")
  expect_error(validate_sample_method_input(n = c(10, 20)), "'n' must be a single non-negative integer")
  expect_error(validate_sample_method_input(n = NA), "'n' must be a single non-negative integer")
})

test_that("validate_sample_method_input handles distribution parameters", {
  # Valid parameters (scalars and vectors)
  expect_silent(validate_sample_method_input(n = 10, mu = 0, sigma = 1))
  expect_silent(validate_sample_method_input(n = 5, shape = c(0.1, 0.5, 0.9)))
  
  # Invalid parameter types
  expect_error(validate_sample_method_input(n = 10, mu = "zero"), "Parameter 'mu' must be numeric")
  
  # Non-finite values
  expect_error(validate_sample_method_input(n = 10, sigma = Inf), "Parameter 'sigma' must be finite")
  expect_error(validate_sample_method_input(n = 10, lambda = NaN), "Parameter 'lambda' contains NA or NaN")
  
  # Vector with one bad value
  expect_error(validate_sample_method_input(n = 10, mu = c(1, 2, NA)), "Parameter 'mu' contains NA or NaN")
})

test_that("test integration of validate_sample_method_input into sample method functions", {
  # sample_norm
  expect_silent(sample_norm(data.frame(flux_mean = c(10, 10), flux_sd = c(5, 5)), "flux", n = 10)) # Valid parameters
  expect_error(sample_norm(data.frame(flux_mean = c(10, NA), flux_sd = c(5, 5)), "flux", n = 10), "Parameter 'mu' contains NA or NaN") # Invalid parameters

  # sample_poisson
  expect_silent(sample_poisson(data.frame(flux_mean = c(10, 10)), "flux", n = 10)) # Valid parameters
  expect_error(sample_poisson(data.frame(flux_mean = c(10, NA)), "flux", n = 10), "Parameter 'lambda' contains NA or NaN") # Invalid parameters

  # sample_beta
  expect_silent(sample_beta(data.frame(p_col_mean = c(0.1, 0.1), p_col_sd = c(0.01, 0.01)), "p_col", n = 10)) # Valid parameters
  expect_error(sample_beta(data.frame(p_col_mean = c(0.1, NA), p_col_sd = c(NA, 0.01)), "p_col", n = 10), "Parameter 'mu' contains NA or NaN") # Invalid parameters

  # sample_nbinom
  expect_silent(sample_nbinom(data.frame(flux_mean = c(10, 10), flux_sd = c(5, 5)), "flux", n = 10)) # Valid parameters
  expect_error(sample_nbinom(data.frame(flux_mean = c(10, NA), flux_sd = c(5, 5)), "flux", n = 10), "Parameter 'mu' contains NA or NaN") # Invalid parameters
})

test_that("sample_norm correctly handles invalid 'df' argument", {
  expect_error(sample_norm(as.matrix(data.frame(flux_mean = c(10, NA), flux_sd = c(5, 5))), "flux", n = 10), "Parameter 'df' must be a data.frame or tibble.")
})

test_that("sample_norm returns values with correct mean and sd", {
  samples <- sample_norm(test_df, "flux", n = 1000)

  # Length of list should be equal to number of rows in dataframe
  obs_length <- length(samples)
  exp_length <- nrow(test_df)
  expect_equal(obs_length, exp_length)

  obs_means <- purrr::map_dbl(samples, mean)
  obs_sds <- purrr::map_dbl(samples, sd)

  exp_means <- test_df$flux_mean
  exp_sds <- test_df$flux_sd

  # Means and sds should be close within tolerance
  expect_equal(obs_means, exp_means, tolerance = 0.2)
  expect_equal(obs_sds, exp_sds, tolerance = 0.2)
})

test_that("sample_poisson returns values with correct mean", {
  samples <- sample_poisson(test_df, "flux", n = 1000)

  # Length of list should be equal to number of rows in dataframe
  obs_length <- length(samples)
  exp_length <- nrow(test_df)
  expect_equal(obs_length, exp_length)

  obs_means <- purrr::map_dbl(samples, mean)
  obs_sds <- purrr::map_dbl(samples, sd)

  exp_means <- test_df$flux_mean
  exp_sds <- sqrt(test_df$flux_mean) # Square root of mean should be equal to sd

  # Means and sds should be close within tolerance
  expect_equal(obs_means, exp_means, tolerance = 0.2)
  expect_equal(obs_sds, exp_sds, tolerance = 0.2)
})

test_that("get_beta_params returns the correct shape parameters given a mean and sd", {
  # Set known inputs
  target_mean <- 0.6
  target_sd <- 0.1

  # Calculate shape parameters
  params <- get_beta_params(target_mean, target_sd)
  a <- params$shape_a
  b <- params$shape_b

  # Calculate mean and SD 
  calculated_mean <- a / (a + b)
    
  # Formula: SD = sqrt( (a*b) / ((a+b)^2 * (a+b+1)) )
  calculated_sd <- sqrt((a * b) / ((a + b)^2 * (a + b + 1)))
  
  # Assertions
  expect_equal(calculated_mean, target_mean)
  expect_equal(calculated_sd, target_sd)
  expect_type(params, "list")
  expect_named(params, c("shape_a", "shape_b"))
})

test_that("sample_beta returns values with correct mean and sd", {
  samples <- sample_beta(test_df, "p_col", n = 1000)

  # Length of list should be equal to number of rows in dataframe
  obs_length <- length(samples)
  exp_length <- nrow(test_df)
  expect_equal(obs_length, exp_length)

  obs_means <- purrr::map_dbl(samples, mean)
  obs_sds <- purrr::map_dbl(samples, sd)

  exp_means <- test_df$p_col_mean
  exp_sds <- test_df$p_col_sd

  # Means and sds should be close within tolerance
  expect_equal(obs_means, exp_means, tolerance = 0.2)
  expect_equal(obs_sds, exp_sds, tolerance = 0.2)
})

test_that("sample_beta correctly a vector of values equal to the man when sd is zero", {
  par_mean <- 0.01
  par_sd <- 0
  reps <- 5

  obs_samples <- sample_beta(data.frame(p_col_mean = par_mean, p_col_sd = par_sd), "p_col", n = reps)[[1]]
  exp_samples <- rep(par_mean, reps)

  expect_equal(obs_samples, exp_samples)
})

test_that("sample_nbinom returns values with correct mean and sd", {
  samples <- sample_nbinom(test_df, "flux", n = 1000)

  # Length of list should be equal to number of rows in dataframe
  obs_length <- length(samples)
  exp_length <- nrow(test_df)
  expect_equal(obs_length, exp_length)

  obs_means <- purrr::map_dbl(samples, mean)
  obs_sds <- purrr::map_dbl(samples, sd)

  exp_means <- test_df$flux_mean
  exp_sds <- test_df$flux_sd

  # Means and sds should be close within tolerance
  expect_equal(obs_means, exp_means, tolerance = 0.2)
  expect_equal(obs_sds, exp_sds, tolerance = 0.2)
})

test_that("get_sample_methods returns the correct mappings", {
  sample_methods <- get_sample_methods()

  # Should be a list
  expect_type(sample_methods, "list")

  # Check expected names
  exp_names <- c("normal", "poisson", "beta", "nbinom")
  expect_setequal(names(sample_methods), exp_names)

  # All elements should be functions
  expect_true(all(purrr::map_lgl(sample_methods, is.function)))
})

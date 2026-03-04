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

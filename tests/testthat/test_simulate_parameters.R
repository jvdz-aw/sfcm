library(testthat)

# Set seed to ensure reproducibility
set.seed(1337)

# Define a test dataframe with two rows
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

test_that("simulate_parameters runs without errors", {

  parameters <- c("flux", "turbs_e", "p_col")
  distributions <- c("poisson", "poisson", "beta")
  n_sims <- 10

  # Should run without errors
  res <- expect_no_error(simulate_parameters(simulation_input = test_df,
                                             parameters = parameters,
                                             distributions = distributions,
                                             n = n_sims))

  # Check whether the correct columns are returned
  exp_cols <- c("species",
                "simulation_id",
                "flux",
                "a_macro",
                "f_prop",
                "h_prop", "h_prop_ref",
                "rotor_d", "rotor_d_ref",
                "turb_dist", "turb_dist_ref",
                "turbs_e", "turbs_e_ref",
                "p_col")
  expect_setequal(names(res), exp_cols)

  # Number of rows should be equal to nrows test_df times n_sims
  exp_nrows <- nrow(test_df) * n_sims
  expect_equal(nrow(res), exp_nrows)

})


test_that("simulate_parameters fails on missing specified distribution", {

  parameters <- c("flux")
  n_sims <- 500

  # Should run without errors
  expect_error(simulate_parameters(
    simulation_input = test_df,
    parameters = parameters,
    distributions = NULL,
    n = n_sims), "Parameters and distributions vectors have different lengths.")

})


test_that("simulate_parameters fails on selected of unsupported parameter to simulate", {

  # Should fail because an unsupported parameter is selected for simulation
  expect_error(simulate_parameters(simulation_input = test_df, parameters = "rotor_d", distributions = "poisson", n = 10), "The following parameter cannot be simulated: rotor_d")

})


test_that("simulate_parameters fails on selecting invalid distribution", {

  # Should fail because the distribution is not correctly mapped to the parameter due to spelling error
  expect_error(simulate_parameters(simulation_input = test_df, parameters = "flux", distributions = "beta", n = 10), "Probability distribution 'beta' is unavailable for parameter 'flux'")

})


test_that("simulate_parameters runs without errors when using a subset of parameters to simulate", {

  # Should run without error
  res <- expect_no_error(simulate_parameters(simulation_input = test_df, parameters = "flux", distributions = "poisson", n = 10))

  # Result should contain a column `flux_samples`
  expect_true("flux" %in% names(res))

  # Try another but with two parameters
  res_two_pars <- expect_no_error(simulate_parameters(simulation_input = test_df, parameters = c("flux", "p_col"),
                                                      distributions = c("poisson", "beta"),
                                                      n = 10))

  # Result should contain two columns: `flux_samples` and `p_col_samples`
  expect_true("flux" %in% names(res_two_pars))
  expect_true("p_col" %in% names(res_two_pars))

})


test_that("calling is_valid_dataframe on simulation input that is a rowwise or grouped tibble correctly returns an error", {

  test_grp_tibble <- tibble::as_tibble(test_df) %>% dplyr::group_by("species")

  expect_error(simulate_parameters(simulation_input = test_grp_tibble, parameters = c("flux"),
                                   distributions = "poisson",
                                   n = 10), "Simulation input is not a dataframe or tibble.")

  test_row_tibble <- tibble::as_tibble(test_df) %>% dplyr::rowwise()

  expect_error(simulate_parameters(simulation_input = test_row_tibble, parameters = c("flux"),
                                   distributions = "poisson",
                                   n = 10), "Simulation input is not a dataframe or tibble.")

})


test_that("calling check_na_cols on simulation input containing NAs correctly returns an error", {

  # Define a test dataframe with two rows and some NAs
  test_na_df <- data.frame(
    species = c(NA, "B"),
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

  expect_error(simulate_parameters(simulation_input = test_na_df, parameters = c("flux"),
                                   distributions = "poisson",
                                   n = 10), "The following columns in simulation input contain NAs: species")

})


test_that("simulate_parameters correctly returns a `model_input` object", {
  parameters <- c("flux", "turbs_e")
  distributions <- c("nbinom", "poisson")
  expect_true(inherits(simulate_parameters(test_df, parameters, distributions), "model_input"))
})
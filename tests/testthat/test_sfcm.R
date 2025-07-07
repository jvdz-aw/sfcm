library(testthat)

# Define parameters for testing
flux <- 17236.1111
a_macro <- 0.6
f_prop <- 1
h_prop <- 0.46961326
h_prop_ref <- 0.67
rotor_d <- 170
rotor_d_ref <- 60
turb_dist <- 628.6667
turb_dist_ref <- 250
turbs_e <- 2
turbs_e_ref <- 4.242641
p_col <- 0.0017

# Create a test model input dataframe
test_model_input <- data.frame(
  flux = flux,
  a_macro = a_macro,
  f_prop = f_prop,
  h_prop = h_prop,
  h_prop_ref = h_prop_ref,
  rotor_d = rotor_d,
  rotor_d_ref = rotor_d_ref,
  turb_dist = turb_dist,
  turb_dist_ref = turb_dist_ref,
  turbs_e = turbs_e,
  turbs_e_ref = turbs_e_ref,
  p_col = p_col
)

# Set seed for testing simulation
set.seed(1337)


test_that("Test a single mortality calculation using FCM", {

  # Run calculation
  mortality <- fcm(flux = flux,
                   a_macro = a_macro,
                   h_prop = h_prop,
                   h_prop_ref = h_prop_ref,
                   rotor_d = rotor_d,
                   rotor_d_ref = rotor_d_ref,
                   turb_dist = turb_dist,
                   turb_dist_ref = turb_dist_ref,
                   turbs_e = turbs_e,
                   turbs_e_ref = turbs_e_ref,
                   p_col = p_col)

  # Define expected mortality
  exp_mort <- 2.553196

  # Compare estimate with expected value with tolerance up to three decimals
  expect_equal(mortality, exp_mort, tolerance = 0.001)
})


test_that("Test run model failure due to missing column in input", {

  # Remove a column from the test model input
  test_model_input_missing_col <- test_model_input[, !names(test_model_input) %in% c("f_prop")]

  # Set number of iterations
  n_iter <- 10

  # Run calculation using test model input with missing column
  expect_error(run_model(model_input = test_model_input_missing_col),
               "Error: the following columns are missing: f_prop")

})


test_that("Test run model failure due to misspelled column name", {

  # Rename a column from the test model input
  test_model_input_misspelled_col <- test_model_input
  names(test_model_input_misspelled_col)[names(test_model_input_misspelled_col) == "a_macro"] <- "A_macro"

  # Set number of iterations
  n_iter <- 10

  # Run calculation using test model input with missing column
  expect_error(run_model(model_input = test_model_input_misspelled_col),
               "Error: the following columns are missing: a_macro")

})


test_that("Test run model failure due to invalid data in column", {

  # Create model input with invalid data
  test_model_input_invalid_data <- test_model_input
  test_model_input_invalid_data$flux <- "15"

  # Set number of iterations
  n_iter <- 10

  # Run calculation using test model input with missing column
  expect_error(run_model(model_input = test_model_input_invalid_data),
               "Error: the following columns contain invalid data: flux")
})


test_that("simulate_parameters output can be used as input for running the model", {

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

  # Simulate parameters
  n_sims <- 5
  parameters <- c("flux", "turbs_e", "p_col")
  model_input <- simulate_parameters(data = test_df,
                                     parameters = parameters,
                                     distributions = list("flux" = "poisson",
                                                          "turbs_e" = "poisson",
                                                          "p_col" = "beta"),
                                     n = n_sims)

  # Simply test whether this runs without errors
  model_res <- expect_no_error(run_model(model_input))

  # Number of rows in model results should be equal to rows in input times the number of sims
  expect_equal(nrow(model_res), nrow(test_df) * n_sims)

  # Check whether the correct columns are returned
  exp_cols <- c(names(dplyr::select(test_df, !tidyr::matches("_mean|_sd"))), "simulation_id", parameters, "mortality")
  expect_setequal(exp_cols, names(model_res))
})

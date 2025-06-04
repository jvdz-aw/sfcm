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

# Test mortality calculation using FCM
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


# Test simulation flux parameter estimates under Poisson distribution
test_that("Test simulation using Poisson distribution in stochastic FCM", {

  # Set number of iterations
  n_iter <- 10000

  # Run calculation using test model input
  mortality <- run_model(model_input = test_model_input,
                         model_type = "sfcm",
                         n_iter = n_iter)

  # The mean and variance of the simulated flux estimates should be approximately
  # equal to the observed flux estimate used as lambda parameter with some tolerance (allow 0.1% difference)
  expect_equal(mean(mortality$flux), flux, tolerance = flux * 0.001)
  expect_equal(var(mortality$flux), flux, tolerance = flux * 0.001)
})


# Test run model failure on missing column
test_that("Test run model failure due to missing column in input", {

  # Remove a column from the test model input
  test_model_input_missing_col <- test_model_input[, !names(test_model_input) %in% c("f_prop")]

  # Set number of iterations
  n_iter <- 10

  # Run calculation using test model input with missing column
  expect_error(run_model(model_input = test_model_input_missing_col,
                         model_type = "sfcm",
                         n_iter = n_iter),
               "Error: the following columns are missing: f_prop")

})

# Test run model failure on misspelled column
test_that("Test run model failure due to misspelled column name", {

  # Rename a column from the test model input
  test_model_input_misspelled_col <- test_model_input
  names(test_model_input_misspelled_col)[names(test_model_input_misspelled_col) == "a_macro"] <- "A_macro"

  # Set number of iterations
  n_iter <- 10

  # Run calculation using test model input with missing column
  expect_error(run_model(model_input = test_model_input_misspelled_col,
                         model_type = "sfcm",
                         n_iter = n_iter),
               "Error: the following columns are missing: a_macro")

})

# Test run model failure on invalid data
test_that("Test run model failure due to invalid data in column", {

  # Create model input with invalid data
  test_model_input_invalid_data <- test_model_input
  test_model_input_invalid_data$flux <- "15"

  # Set number of iterations
  n_iter <- 10

  # Run calculation using test model input with missing column
  expect_error(run_model(model_input = test_model_input_invalid_data,
                         model_type = "sfcm",
                         n_iter = n_iter),
               "Error: the following columns contain invalid data: flux")
})

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

test_that("run_model raises an error when not providing a model_input object as input", {
  exp_err_msg <- "Model input is not a `model_input` object."
  expect_error(run_model(test_model_input), exp_err_msg)
})
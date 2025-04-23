library(testthat)

# Define parameters for testing
flux <- 17236.1111
a_macro <- 0.6
h_prop <- 0.46961326
h_prop_ref <- 0.67
rotor_d <- 170
rotor_d_ref <- 60
turb_dist <- 628.6667
turb_dist_ref <- 250
turbs_e <- 2
turbs_e_ref <- 4.242641
p_col <- 0.0017

# Test numeric input checking failure
test_that("Test numeric input checking failure", {

  # Define bad parameter input
  bad_flux <- "17236.1111"

  # Test expected failure
  expect_error(
    fcm(flux = bad_flux,
      a_macro = a_macro,
      h_prop = h_prop,
      h_prop_ref = h_prop_ref,
      rotor_d = rotor_d,
      rotor_d_ref = rotor_d_ref,
      turb_dist = turb_dist,
      turb_dist_ref = turb_dist_ref,
      turbs_e = turbs_e,
      turbs_e_ref = turbs_e_ref,
      p_col = p_col),
    regexp = "Error: parameter flux must be a positive number.") # Check error message

})


# Test numeric range input checking failure
test_that("Test numeric range input checking failure", {

  # Define bad parameter input
  bad_p_col <- 1.2

  # Test expected failure
  expect_error(
    fcm(flux = bad_p_col,
        a_macro = a_macro,
        h_prop = h_prop,
        h_prop_ref = h_prop_ref,
        rotor_d = rotor_d,
        rotor_d_ref = rotor_d_ref,
        turb_dist = turb_dist,
        turb_dist_ref = turb_dist_ref,
        turbs_e = turbs_e,
        turbs_e_ref = turbs_e_ref,
        p_col = bad_p_col),
    regexp = "Error: parameter p_col must be a number between 0 and 1.") # Check error message
})


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

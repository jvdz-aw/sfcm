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


# Test numeric column input check success
test_that("Test numeric column input check success", {
  col_vals <- c(10,15,20)
  col_name <- "valid_numeric_col"
  expect_false(check_col_num(col_vals, col_name))
})


# Test numeric column input check failure
test_that("Test numeric column input check failure", {
  col_vals_negative <- c(10, -10, -5)
  col_vals_string <- c("10", "15", "20")
  col_name <- "invalid_numeric_col"
  expect_true(check_col_num(col_vals_negative, col_name))
  expect_true(check_col_num(col_vals_string, col_name))
})


# Test numeric column range input check success
test_that("Test numeric column range input check success", {
  col_values <- c(0.5, 0.1, 0.3)
  expect_false(check_col_numrange(col_values))
})


# Test numeric column range input check failure
test_that("Test numeric range input check failure", {
  col_vals_negative <- c(-0.2, 10, 0.5)
  col_vals_string <- c("0.5", "0.1", "0.3")
  col_min <- 0
  col_max <- 1
  col_name <- "invalid_numrange_col"
  expect_error(check_col_numrange(col_vals_negative, col_min, col_max))
  expect_error(check_col_numrange(col_vals_string, col_min, col_max))
})


# Test model input checking failure on missing or misspelled columns
test_that("Test model input checking failure on missing or misspelled columns", {

  # Create a test model input dataframe containing invalid column names
  test_model_input_invalid_cols <- data.frame(
    flu = flux,
    amacro = a_macro,
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

  # Three columns are invalid, check whether the expected error is returned
  expect_error(check_model_input(test_model_input_invalid_cols), "Error: the following columns are missing: flux, a_macro, f_prop")
})


test_that("Test model input checking failure on invalid data in columns", {

  # Create a test model input dataframe containing some invalid data
  test_model_input_invalid_data <- data.frame(
    flux = "15",
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
    p_col = -0.3 # Invalid
  )

  # Flux and p_col columns contain invalid data, check whether expected error is returned
  expect_error(check_model_input(test_model_input_invalid_data), "Error: the following columns contain invalid data: flux, p_col")
})

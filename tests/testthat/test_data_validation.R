library(testthat)

test_that("validate_model_input throws an error if columns are missing", {

  model_input_missing_cols <- data.frame(flux = 10, a_macro = 0.95)
  expect_error(validate_model_input(model_input_missing_cols), "Error: not all required columns are present in the model input.")

})


test_that("validate_model_input throws an error when providing invalid column values", {

  model_input_invalid_vals <- data.frame(flux = -10,
                                         a_macro = 2,
                                         f_prop = 1,
                                         h_prop = 0.8,
                                         h_prop_ref = 0.6,
                                         rotor_d = 100,
                                         rotor_d_ref = 80,
                                         turb_dist = 250,
                                         turb_dist_ref = 200,
                                         turbs_e = 2,
                                         turbs_e_ref = 1.5,
                                         p_col = 4)

  expect_error(validate_model_input(model_input_invalid_vals), "Error: some columns contain invalid data.")

})

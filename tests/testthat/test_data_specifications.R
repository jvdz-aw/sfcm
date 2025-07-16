library(testthat)
library(purrr)

test_that("model_input_data_spec returns the expected column names and contains functions", {

  data_spec <- model_input_data_spec()
  obs_cols <- names(data_spec)
  exp_cols <- c("flux",
                "a_macro",
                "f_prop",
                "h_prop", "h_prop_ref",
                "rotor_d", "rotor_d_ref",
                "turb_dist", "turb_dist_ref",
                "turbs_e", "turbs_e_ref",
                "p_col")

  # Observed columns should be in expected columns and vice versa
  expect_setequal(obs_cols, exp_cols)

  # All columns contain functions
  expect_true(all(map_lgl(data_spec, is.function)))

})

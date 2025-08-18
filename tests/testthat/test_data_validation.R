library(testthat)

test_that("validate_model_input throws an error if columns are missing", {

  model_input_missing_cols <- data.frame(flux = 10, a_macro = 0.95)
  expect_error(validate_model_input(model_input_missing_cols), "the following columns are missing: f_prop, h_prop, h_prop_ref, rotor_d, rotor_d_ref, turb_dist, turb_dist_ref, turbs_e, turbs_e_ref, p_col")

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

  expect_error(validate_model_input(model_input_invalid_vals), "the following columns contain invalid data: flux, a_macro, p_col")

})


test_that("validate_model_input runs successfully on simulation output", {

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

  model_input <- simulate_parameters(simulation_input = test_df,
                                     parameters = "flux",
                                     distributions = "poisson",
                                     n = 5)

  expect_no_error(validate_model_input(model_input))

})


test_that("is_valid_dataframe succeeds on valid dataframe", {

  test_df <- data.frame(
    species = c("A", "B")
  )

  expect_true(is_valid_dataframe(test_df))

  test_tibble <- tibble::tibble(
    species = c("A", "B")
  )

  expect_true(is_valid_dataframe(test_tibble))
})


test_that("is_valid_dataframe fails on invalid dataframe", {

  test_grouped_tibble <- tibble::tibble(
    species = c("A", "B")
  ) %>% dplyr::group_by(species)

  expect_false(is_valid_dataframe(test_grouped_tibble))

  test_rowwise_tibble <- tibble::tibble(
    species = c("A", "B")
  ) %>% dplyr::rowwise()

  expect_false(is_valid_dataframe(test_rowwise_tibble))

})


test_that("check_na_cols detects columns containing NAs correctly", {

  # Both columns contain NA
  test_df <- data.frame(
    species = c("A", NA),
    a_macro = c(NA, 0.95)
  )

  # Should return a vector containing two times TRUE
  expect_setequal(check_na_cols(test_df), c(TRUE, TRUE))

})

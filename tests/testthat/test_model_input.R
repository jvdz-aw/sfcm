library(testthat)

test_tibble <- tibble::tibble(
  x = c(1, 2, 3),
  y = c(4, 5, 6)
)

test_that("new_model_input sets model_input class attribute correctly", {
  test_model_input <- new_model_input(test_tibble)
  expect_true(inherits(test_model_input, "model_input"))
})

test_that("validate_model_input raises error on non-tibble input", {
  expect_error(validate_model_input(as.data.frame(test_tibble)), "`x` must be a tibble.")
})

test_that("validate_model_input raises error on missing columns in tibble input", {
  exp_err_msg <- "`x` is missing the following required columns: flux, a_macro, f_prop, h_prop, h_prop_ref, rotor_d, rotor_d_ref, turb_dist, turb_dist_ref, turbs_e, turbs_e_ref, p_col"
  expect_error(validate_model_input(test_tibble), exp_err_msg)
})

test_that("validate_model_input raises error on columns with invalid numeric values in tibble input", {
  test_tibble_invalid_nums <- tibble::tibble(
    species = "A",
    flux = -10,
    a_macro = 0.95,
    f_prop = 1,
    h_prop = 0.8,
    h_prop_ref = 0.6,
    rotor_d = 100,
    rotor_d_ref = 80,
    turb_dist = 250,
    turb_dist_ref = 200,
    turbs_e = 2,
    turbs_e_ref = 1.5,
    p_col = 0.8
  )

  exp_err_msg <- "`x` contains invalid numeric values in the following columns: flux"
  expect_error(validate_model_input(test_tibble_invalid_nums), exp_err_msg)
})

test_that("validate_model_input raises error on columns with invalid numeric range values in tibble input", {
  test_tibble_invalid_numrange <- tibble::tibble(
    species = "A",
    flux = 10,
    a_macro = 9.5,
    f_prop = 1,
    h_prop = 0.8,
    h_prop_ref = 0.6,
    rotor_d = 100,
    rotor_d_ref = 80,
    turb_dist = 250,
    turb_dist_ref = 200,
    turbs_e = 2,
    turbs_e_ref = 1.5,
    p_col = 4
  )

  exp_err_msg <- "`x` contains invalid numeric range values in the following columns: a_macro, p_col"
  expect_error(validate_model_input(test_tibble_invalid_numrange), exp_err_msg)
})

test_that("model_input creates new class object succesfully", {
  test_df <- data.frame(
    species = c("A", "B"),
    flux = c(10, 15),
    a_macro = 0.95,
    f_prop = 1,
    h_prop = 0.8,
    h_prop_ref = 0.6,
    rotor_d = 100,
    rotor_d_ref = 80,
    turb_dist = 250,
    turb_dist_ref = 200,
    turbs_e = 2,
    turbs_e_ref = 1.5,
    p_col = c(0.8, 0.6))

    expect_false(inherits(test_df, "model_input"))
    expect_true(inherits(model_input(test_df), "model_input"))
})
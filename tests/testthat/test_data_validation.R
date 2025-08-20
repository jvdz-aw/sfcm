library(testthat)

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

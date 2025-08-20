#' Check numeric columns
#'
#' Internal function that checks whether a column contains positive numbers.
#'
#' @returns TRUE if values are all positive numeric and FALSE if not
#' @keywords internal
is_valid_numcol <- function(col_values) {
  is.numeric(col_values) & all(col_values >= 0) & all(!is.na(col_values))
}


#' Check numeric range columns
#'
#' This helper function checks whether a columns contains positive numbers between 0 and 1.
#'
#' @returns TRUE if values are all positive numeric between 0 and 1, FALSE if not
#' @keywords internal
is_valid_numrange <- function(col_values) {
  is.numeric(col_values) & all(col_values >= 0) & all(col_values <= 1) & all(!is.na(col_values))
}


#' Check dataframe
#'
#' Internal function that checks whether a dataframe is a dataframe or a tibble.
#'
#' @importFrom dplyr is_grouped_df
#' @keywords internal
is_valid_dataframe <- function(df) {
  is.data.frame(df) && (
    !is_grouped_df(df) && !inherits(df, "rowwise_df")
  )
}


#' Check for NA in dataframe columns
#'
#' Internal function that checks whether dataframe columns contain NAs.
#'
#' @importFrom purrr map_lgl
#' @keywords internal
check_na_cols <- function(df) {
  map_lgl(df, \(x) any(is.na(x)))
}


#' Validate simulation input
#'
#' Internal function that validates simulation input.
#' @keywords internal
validate_simulation_input <- function(simulation_input) {

  # Check dataframe type
  if (!is_valid_dataframe(simulation_input)) {
    stop("Simulation input is not a dataframe or tibble.")
  }

  # Check for NAs in columns
  na_cols <- check_na_cols(simulation_input)
  if (any(na_cols)) {
    stop(paste("The following columns in simulation input contain NAs:", names(simulation_input)[na_cols]))
  }
}
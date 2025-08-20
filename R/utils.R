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


#' Determine simulation output columns to be removed/renamed
#'
#' This internal functions determines which columns should be removed or renamed
#' in simulation output based upon which parameters were selected for simulation.
#'
#' @returns A named list containing two string vectors with columns to be removed or renamed.
#' @keywords internal
get_cols_remove_rename <- function(parameters) {

 # Get all parameters and determine which weren't simulated
  all_pars <- c("flux", "turbs_e", "p_col")
  unsim_pars <- all_pars[!all_pars %in% parameters]

  # Determine columns to be removed
  cols_to_remove <- c(paste0(all_pars, "_sd"), paste0(parameters, "_mean"))

  # Determine columns to be renamed
  if (length(unsim_pars) > 0) { # Only meaningful to include un-simulated parameters in output if they are actually there
    cols_to_rename <- c(paste0(unsim_pars, "_mean"), paste0(parameters, "_samples"))
  } else {
    cols_to_rename <- c(paste0(parameters, "_samples"))
  }

  # Wrap in named list
  list(
    to_remove = cols_to_remove,
    to_rename = cols_to_rename
    )
}



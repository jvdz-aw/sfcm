#' Check numeric columns
#'
#' @description
#' Internal function that checks whether a column contains positive numbers.
#'
#' @param col_values A vector.
#' 
#' @returns TRUE if values are all positive numeric and FALSE if not.
#' 
#' @keywords internal
is_valid_numcol <- function(col_values) {
  is.numeric(col_values) & all(col_values > 0) & all(!is.na(col_values))
}


#' Check numeric range columns
#'
#' @description
#' This helper function checks whether a columns contains positive numbers between 0 and 1.
#' 
#' @param col_values A vector.
#' 
#' @returns TRUE if values are all positive numeric between 0 and 1, FALSE if not.
#' 
#' @keywords internal
is_valid_numrange <- function(col_values) {
  is.numeric(col_values) & all(col_values > 0) & all(col_values <= 1) & all(!is.na(col_values))
}


#' Check dataframe
#'
#' @description
#' Internal function that checks whether a dataframe is a data.frame or a tibble.
#'
#' @param df A dataframe.
#' 
#' @returns
#' TRUE if a dataframe is a data.frame or tibble. FALSE if dataframe is a grouped-, or rowwise tibble.
#' 
#' @importFrom dplyr is_grouped_df
#' 
#' @keywords internal
is_valid_dataframe <- function(df) {
  is.data.frame(df) && (
    !is_grouped_df(df) && !inherits(df, "rowwise_df")
  )
}


#' Check for NA in dataframe columns
#'
#' @description
#' Internal function that checks whether dataframe columns contain NAs.
#'
#' @param df A dataframe.
#' 
#' @returns
#' A vector of booleans (i.e., TRUE or FALSE) indicating whether a column contains NAs.
#' 
#' @importFrom purrr map_lgl
#' 
#' @keywords internal
check_na_cols <- function(df) {
  map_lgl(df, \(x) any(is.na(x)))
}

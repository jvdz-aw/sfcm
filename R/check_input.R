#' Check numeric columns
#'
#' @description This helper function checks whether a column contains positive
#' numeric data. Returns FALSE if data is valid, and TRUE if data is invalid.
check_col_num <- function(col_values, col_name) {
  any(!is.numeric(col_values)) | any(col_values < 0)
}


#' Check numeric range columns
#'
#' @description This helper function checks whether a columns contains positive
#' numbers in the range [0, 1]. Returns FALSE if data is valid, and TRUE if data is invalid.
check_col_numrange <- function(col_values) {
  any(!is.numeric(col_values)) | any(col_values < 0) | any(col_values > 1)
}

#' Check model input
#'
#' @description This helper function wraps functions for checking whether column
#' values are valid and applies these to the model input dataframe
check_model_input <- function(model_input) {

  # Define vector containing the columns that need to be present in the model input dataframe
  required_cols <- c("flux",
                     "a_macro",
                     "f_prop",
                     "h_prop", "h_prop_ref",
                     "rotor_d", "rotor_d_ref",
                     "turb_dist", "turb_dist_ref",
                     "turbs_e", "turbs_e_ref",
                     "p_col")

  # Check whether there are missing columns
  missing_cols <- required_cols[!required_cols %in% names(model_input)]
  if (length(missing_cols) > 0) {
    stop(paste("Error: the following columns are missing:", paste(missing_cols, collapse = ", ")))
  }

  # Check whether data in numeric columns is valid
  num_cols <- c("flux", "rotor_d", "rotor_d_ref", "turb_dist", "turb_dist_ref", "turbs_e", "turbs_e_ref")
  invalid_num_cols <- num_cols[sapply(model_input[num_cols], check_col_num)]

  # Check whether data in numeric range columns is valid
  numrange_cols <- c("a_macro", "f_prop", "h_prop", "h_prop_ref", "p_col")
  invalid_numrange_cols <- numrange_cols[sapply(model_input[numrange_cols], check_col_numrange)]

  invalid_cols <- c(invalid_num_cols, invalid_numrange_cols)

  if (length(invalid_cols) > 0) {
    stop(paste("Error: the following columns contain invalid data:", paste(invalid_cols, collapse = ", ")))
  }
}

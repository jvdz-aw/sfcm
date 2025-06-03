#' Check numeric arguments
#'
#' @description This helper function checks whether an argument is a positive
#' number. Stops execution and prints the argument name to the user if an invalid
#' value is provided.
check_arg_num <- function(arg, arg_name) {
  if (!is.numeric(arg) || arg <= 0) {
    stop(paste("Error: parameter", arg_name, "must be a positive number."))
  }
}


#' Check numeric range arguments
#'
#' @description This helper function checks whether an argument is a positive
#' number in the range [arg_min, arg_max]. Stops execution and prints the argument
#' name to the user if an invalid value is provided.
check_arg_numrange <- function(arg, arg_min, arg_max, arg_name) {
  if (!is.numeric(arg) || arg < arg_min || arg > arg_max) {
    stop(paste("Error: parameter", arg_name,"must be a number between", arg_min, "and", arg_max, "."))
  }
}


#' Check numeric columns
#'
#' @description This helper function checks whether a column contains positive
#' numeric data. Stops execution and prints the column name to the user if a
#' column contains invalid data.
check_col_num <- function(col_values, col_name) {
  if (any(!is.numeric(col_values)) | any(col_values < 0)) {
    stop(paste("Error: column", col_name, "must contain positive numbers."))
  }
}


#' Check numeric range columns
#'
#' @description This helper function checks whether a columns contains positive
#' numbers in the range [arg_min, arg_max]. Stops execution and prints the column
#' name to the user if it contains invalid values.
check_col_numrange <- function(col_values, col_min, col_max, col_name) {
  if (any(!is.numeric(col_values)) | any(col_values < col_min) | any(col_values > col_max)) {
    stop(paste("Error: column", col_name,"must contain numbers between", col_min, "and", col_max, "."))
  }
}

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

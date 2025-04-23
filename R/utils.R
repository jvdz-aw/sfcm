#' Get argument name
#'
#' @description This helper function is used internally by the
#' @param arg A function argument
#' @returns The name of the argument as a string.
get_arg_name <- function(arg) {
  deparse(substitute(arg))
}


#' Check numeric arguments
#'
#' @description TODO
check_arg_num <- function(arg) {
  if (!is.numeric(arg) || arg <= 0) {
    stop(paste("Error: parameter", get_arg_name(arg), "must be a positive number."))
  }
}


#' Check numeric range arguments
#'
#' @description TODO
check_arg_numrange <- function(arg, arg_min, arg_max) {
  if (!is.numeric(arg) || arg < arg_min || arg > arg_max) {
    stop(paste("Error: parameter", get_arg_name(arg) ,"must be number between", get_arg_name(arg_min), "and", get_arg_name(arg_max), "."))
  }
}

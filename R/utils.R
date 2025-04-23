#' Check numeric arguments
#'
#' @description TODO
check_arg_num <- function(arg, arg_name) {
  if (!is.numeric(arg) || arg <= 0) {
    stop(paste("Error: parameter", arg_name, "must be a positive number."))
  }
}


#' Check numeric range arguments
#'
#' @description TODO
check_arg_numrange <- function(arg, arg_min, arg_max, arg_name) {
  if (!is.numeric(arg) || arg < arg_min || arg > arg_max) {
    stop(paste("Error: parameter", arg_name,"must be number between", arg_min, "and", arg_max, "."))
  }
}

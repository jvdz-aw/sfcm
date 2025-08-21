#' Construct a new `model_input` object
#' 
#' @description
#' Internal low-level constructor for the `model_input` S3 class. A `model_input` is a
#' tibble with additional validation checks to ensure the data conforms to
#' the expected structure for use in a model.
#' 
#' @details
#' This function should generally not be called directly by users.
#' Instead, use [model_input()] which provides a user-friendly interface.
#'
#' @param x A [tibble::tibble] object containing the model input data.
#'
#' @returns A `model_input` object (inherits from tibble).
#'
#' @keywords internal
new_model_input <- function(x = tibble::tibble()) {
  stopifnot(tibble::is_tibble(x))
  structure(x, class = c("model_input", class(x)))
}


#' Validate a `model_input` object
#'
#' @description
#' Internal validation function for the `model_input` class. Ensures that the
#' object is a tibble and that it conforms to structural requirements, such
#' as containing required columns or valid values.
#'
#' @details
#' This function is typically called inside [model_input()] and should not
#' need to be called directly by users.
#'
#' @param x A `model_input` object to validate.
#'
#' @returns A validated `model_input` object.
#'
#' @keywords internal
validate_model_input <- function(x) {

  if (!tibble::is_tibble(x)) {
    stop("`x` must be a tibble.", call. = FALSE)
  }

  required_cols <- c(
    "flux", 
    "a_macro", 
    "f_prop",
    "h_prop", "h_prop_ref",
    "rotor_d", "rotor_d_ref", 
    "turb_dist", "turb_dist_ref", 
    "turbs_e", "turbs_e_ref",
    "p_col"
  )

  if (!all(required_cols %in% names(x))) {
    stop("`x` is missing the following required columns: ", paste0(setdiff(required_cols, names(x)), collapse = ", "), call. = FALSE)
  }
  
  num_cols <- c("flux", "rotor_d", "rotor_d_ref", "turb_dist", "turb_dist_ref", "turbs_e", "turbs_e_ref")
  valid_numcols <- purrr::map_lgl(x[num_cols], is_valid_numcol)
  if (!all(valid_numcols)) {
    stop("`x` contains invalid numeric values in the following columns: ", paste0(num_cols[!valid_numcols], collapse = ", "), call. = FALSE)
  }

  numrange_cols <- c("a_macro", "f_prop", "h_prop", "h_prop_ref", "p_col")
  valid_numrange <- purrr::map_lgl(x[numrange_cols], is_valid_numrange)
  if (!all(valid_numrange)) {
    stop("`x` contains invalid numeric range values in the following columns: ", paste0(numrange_cols[!valid_numrange], collapse = ", "), call. = FALSE)
  }

  x
}


#' Create a `model_input` tibble
#'
#' @description
#' High-level helper for creating a `model_input` object. This function
#' constructs a tibble from the provided columns and applies the class
#' constructor and validation.
#'
#' @param ... Named columns passed to [tibble::tibble()].
#'
#' @returns A `model_input` object (inherits from tibble).
#'
#' @examples
#' df <- model_input(data.frame(
#'   species = c("A", "B"),
#'   flux = c(10, 15),
#'   a_macro = 0.95,
#'   f_prop = 1,
#'   h_prop = 0.8,
#'   h_prop_ref = 0.6,
#'   rotor_d = 100,
#'   rotor_d_ref = 80,
#'   turb_dist = 250,
#'   turb_dist_ref = 200,
#'   turbs_e = 2,
#'   turbs_e_ref = 1.5,
#'   p_col = c(0.8, 0.6))
#' df
#' class(df)
#'
#' @importFrom tibble tibble
#' @export
model_input <- function(...) {
  x <- tibble(...)
  validate_model_input(new_model_input(x))
}
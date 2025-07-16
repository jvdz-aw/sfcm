#' Model input data specification
#'
#' Internal function that defines the structure of the model input data.
#'
#' @returns A named list mapping required columns to internal data validation functions for
#' model input data.
#' @keywords internal
model_input_data_spec <- function() {
  list(
    "flux" = is_valid_numcol,
    "a_macro" = is_valid_numrange,
    "f_prop" = is_valid_numrange,
    "h_prop" = is_valid_numrange,
    "h_prop_ref" = is_valid_numrange,
    "rotor_d" = is_valid_numcol,
    "rotor_d_ref" = is_valid_numcol,
    "turb_dist" = is_valid_numcol,
    "turb_dist_ref" = is_valid_numcol,
    "turbs_e" = is_valid_numcol,
    "turbs_e_ref" = is_valid_numcol,
    "p_col" = is_valid_numrange
  )
}

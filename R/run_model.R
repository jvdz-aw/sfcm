#' Run the flux collision model on a dataframe of model input parameters
#'
#' @description Estimate bird mortality due to collisions with wind turbines using either
#' the stochastic or non-stochastic version of the flux collision model.
#'
#' @param model_input A `model_input` object containing input parameters for the flux collision model.
#' @returns
#' A dataframe similar to `model_input` that includes a column `mortality` containing bird mortality estimates.
#' @importFrom dplyr mutate
#' @importFrom purrr pmap_dbl
#' @export
run_model <- function(model_input) {
  if (!inherits(model_input, "model_input")) {
    stop("Model input is not a `model_input` object.")
  } else {
    model_input %>%
      mutate(mortality = pmap_dbl(list(flux = flux,
                                      a_macro = a_macro,
                                      h_prop = h_prop,
                                      h_prop_ref = h_prop_ref,
                                      rotor_d = rotor_d,
                                      rotor_d_ref = rotor_d_ref,
                                      turb_dist = turb_dist,
                                      turb_dist_ref = turb_dist_ref,
                                      turbs_e = turbs_e,
                                      turbs_e_ref = turbs_e_ref,
                                      p_col = p_col), fcm))
  }
}
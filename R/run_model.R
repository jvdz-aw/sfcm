#' Run the FCM on model input
#'
#' @description 
#' Run the Flux Collision Model (FCM) to estimate bird collisions from input parameter values stored in `model_input` dataframes.
#' 
#' @details
#' This function is a wrapper of [fcm()] that provides an easy way of estimating the number of bird collisions for large sets
#' of input parameter values. In addition, it checks whether the input parameter values are valid by through the use of the `model_input` 
#' S3 class for its input.
#'
#' @param model_input A `model_input` object containing input parameters for the flux collision model.
#' 
#' @returns
#' A `model_input` object that includes a column `n_collisions` with the estimated number of bird collisions.
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
#' 
#' res <- run_model(df)
#' print(res)
#' 
#' @seealso [model_input()], [fcm()]
#' 
#' @importFrom dplyr mutate
#' @importFrom purrr pmap_dbl
#' 
#' @export
run_model <- function(model_input) {
  if (!inherits(model_input, "model_input")) {
    stop("Model input is not a `model_input` object.")
  } else {
    model_input %>%
      mutate(n_collisions = pmap_dbl(list(flux = flux,
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
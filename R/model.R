#' Definition of the Flux Collision Model.
#'
#' @description This function represents the core definition of the Flux Collision
#' Model from Kleyheeg-Hartman et al. (2018).
#' @param flux Number of bird movements through planned wind farm
#' @param a_macro Proportion of birds avoiding the entire wind farm
#' @param f_prop Proportion of flux going through the wind farm. The default value is 1, which means
#' all of the estimated flux goes through a wind farm.
#' @param h_prop Proportion of flux at rotor height in the planned wind farm
#' @param h_prop_ref Proportion of flux at rotor height in the reference wind farm
#' @param rotor_d Rotor diameter of turbines used in the planned wind farm
#' @param rotor_d_ref Rotor diameter of turbines used in the reference wind farm
#' @param turb_dist Mean distance among turbines in the planned wind farm
#' @param turb_dist_ref Mean distance among turbines in the reference wind farm
#' @param turbs_e Mean number of turbines encountered by birds in the planned wind farm
#' @param turbs_e_ref Mean number of turbines encountered by birds in the reference wind farm
#' @param p_col Collision probability estimated in the reference wind farm
#' @export
fcm <- function(flux, a_macro,
                f_prop = 1,
                h_prop, h_prop_ref,
                rotor_d, rotor_d_ref,
                turb_dist, turb_dist_ref,
                turbs_e, turbs_e_ref,
                p_col, ...) {

  # calculate flux at rotor height using h * (h / h_ref)
  flux_rotor_height <- (h_prop) / h_prop_ref

  # calculate correction factor difference in turbine dimensions
  turb_corr <- (rotor_d * (turb_dist)^-1) / (rotor_d_ref * (turb_dist_ref)^-1)

  # calculate correction factor difference in encounter rate
  e_corr <- turbs_e / turbs_e_ref

  # calculate corrected collision probability
  p_col_corr <- (0.9785 * (rotor_d^2 / rotor_d_ref^2))^-0.26

  # calculate mortality defined as expected number of collisions
  mort <- flux * (1-a_macro) * f_prop * flux_rotor_height * turb_corr * e_corr * p_col * p_col_corr

  # return mortality
  return(mort)
}


#' Run the flux collision model on a dataframe of model input parameters
#'
#' @description Estimate bird mortality due to collisions with wind turbines using either
#' the stochastic or non-stochastic version of the flux collision model.
#'
#' @param model_input A dataframe containing input parameters for the flux collision model.
#' @returns
#' A dataframe similar to `model_input` that includes a column `mortality` containing bird mortality estimates.
#' @importFrom dplyr mutate
#' @importFrom purrr pmap_dbl
#' @export
run_model <- function(model_input) {

  # Check model input
  validate_model_input(model_input)

  # Apply function to each row in model input dataframe
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

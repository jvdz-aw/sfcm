#' Flux Collision Model
#'
#' Estimate bird mortality due to collisions with wind turbines using the Flux Collision Model.
#'
#' @description A simplified implementation of the Flux Collision Model from Kleyheeg-Hartman et al. (2018)
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

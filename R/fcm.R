#' Flux Collision Model.
#'
#' @description 
#' The Flux Collision Model (FCM) is an empirical collision risk model that uses knowledge of species (group)-specific collision
#' probabilties estimated in existing wind farms to calculate collision rates for planned wind farms (Kleyheeg-Hartman et al. 2018). 
#' In general, we recommend users to use [run_model()] since this comes with some built-in checks of the input parameter
#' values and presents an easy-to-use interface for running the FCM on large sets of input parameters stored in `model_input` objects.
#' 
#' @details
#' The implementation of the FCM in this R package is a slightly modified version of the original FCM. The original FCM had 
#' additional parameters but could be further mathematically solved, resulting in the simplified FCM (with fewer parameters)
#' implemented in this R package.
#' 
#' @param flux The number of bird movements through the planned wind farm. Should be a positive numeric value.
#' @param a_macro The proportion of birds avoiding the planned wind farm. Should be a positive numeric value between 0 and 1.
#' @param f_prop The proportion of flux going through the wind farm. Should be a positive numeric value between 0 and 1 
#' (default = 1). We genrally recommend using the default value, unless users have strong reasons to believe only part of the estimated
#' flux is through the planned wind farm.
#' @param h_prop The proportion of flux at rotor height in the planned wind farm. Should be a positive numeric value between 0 and 1.
#' @param h_prop_ref The proportion of flux at rotor height in the reference wind farm. Should be a positive numeric value between 0 and 1.
#' @param rotor_d The rotor diameter of turbines used in the planned wind farm. Should be a positive numeric value.
#' @param rotor_d_ref The rotor diameter of turbines used in the reference wind farm. Should be a positive numeric value.
#' @param turb_dist The distance among turbines in the planned wind farm. Should be a positive numeric value.
#' @param turb_dist_ref The distance among turbines in the reference wind farm. Should be a positive numeric value.
#' @param turbs_e The number of turbines encountered by birds in the planned wind farm. Should be a positive numeric value.
#' @param turbs_e_ref The number of turbines encountered by birds in the reference wind farm. Should be a positive numeric value.
#' @param p_col The collision probability estimated in the reference wind farm. Should be a positive numeric value between 0 and 1.
#' 
#' @returns
#' An estimate of the number of bird collisions expected in the planned wind farm.
#' 
#' @examples
#' n_collisions <- fcm(flux = 10,
#'                     a_macro = 0.95,
#'                     f_prop = 1,
#'                     h_prop = 0.8,
#'                     h_prop_ref = 0.6,
#'                     rotor_d = 100,
#'                     rotor_d_ref = 80,
#'                     turb_dist = 300,
#'                     turb_dist_ref = 200,
#'                     turbs_e = 2,
#'                     turbs_e_ref = 1,
#'                     p_col = 0.7)
#' print(n_collisions)
#' 
#' @seealso [model_input()], [run_model()]
#' 
#' @references
#' Kleyheeg-Hartman et al. (2018). Predicting bird collisions with wind turbines: Comparison of the new empirical Flux Collision Model with the SOSS Band model. \emph{Ecological Modelling}, \strong{387}, 144-153.
#' 
#' @export
fcm <- function(flux, a_macro,
                f_prop = 1,
                h_prop, h_prop_ref,
                rotor_d, rotor_d_ref,
                turb_dist, turb_dist_ref,
                turbs_e, turbs_e_ref,
                p_col, ...) {

  # Flux at rotor height is given by (h / h_ref)
  flux_rotor_height <- (h_prop) / h_prop_ref

  # Correction factor difference in turbine dimensions
  turb_corr <- (rotor_d * (turb_dist)^-1) / (rotor_d_ref * (turb_dist_ref)^-1)

  # Correction factor difference in encounter rate
  e_corr <- turbs_e / turbs_e_ref

  # Correction factor collision probability
  p_col_corr <- (0.9785 * (rotor_d^2 / rotor_d_ref^2))^-0.26

  # Estimate expected number of bird collisions
  flux * (1-a_macro) * f_prop * flux_rotor_height * turb_corr * e_corr * p_col * p_col_corr
}
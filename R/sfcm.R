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

  # Check numeric args
  check_arg_num(arg = flux, arg_name = "flux")
  check_arg_num(arg = rotor_d, arg_name = "rotor_d")
  check_arg_num(arg = turb_dist, arg_name = "turb_dist")
  check_arg_num(arg = turb_dist_ref, arg_name = "turb_dist_ref")
  check_arg_num(arg = turbs_e, arg_name = "turbs_e")
  check_arg_num(arg = turbs_e_ref, arg_name = "turbs_e_ref")

  # Check numeric range args (in range [0, 1])
  check_arg_numrange(arg = a_macro, arg_name = "a_macro", arg_min = 0, arg_max = 1)
  check_arg_numrange(arg = f_prop, arg_name = "f_prop", arg_min = 0, arg_max = 1)
  check_arg_numrange(arg = h_prop, arg_name = "h_prop", arg_min = 0, arg_max = 1)
  check_arg_numrange(arg = h_prop_ref, arg_name = "h_prop_ref", arg_min = 0, arg_max = 1)
  check_arg_numrange(arg = p_col, arg_name = "p_col", arg_min = 0, arg_max = 1)

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


#' Run non-stochastic or stochastic variant of the flux collision model
#'
#' @description Estimate bird mortality due to collisions with wind turbines using either
#' the non-stochastic or stochastic version of the flux collision model. The stochastic
#' variant allows the incorporation of uncertainty in the flux parameter using a simulation
#' approach. Parameter uncertainty is modeled by drawing parameter values from
#' a probability distribution. Currently supports simulation of flux estimates using
#' a Poisson distribution.
#' @param model_input A dataframe or tibble containing input parameters for the flux collision model.
#' @param model_type A string indicating whether to run the stochastic variant (default: `sfcm`) or
#' non-stochastic variant (`fcm`).
#' @param n_iter The number of iterations (default: 10000) when running the stochastic flux collision
#' model.
#' @importFrom dplyr mutate rowwise relocate
#' @importFrom tidyr unnest
#' @export
run_model <- function(model_input, model_type = "sfcm", n_iter = 10000) {

  # TODO move input checks here instead of in model definition

  # First check whether stochastic variant was selected, for which simulations need to be added to input
  if (model_type == "sfcm") {
    model_input <- model_input %>%
      mutate(n_iter = n_iter) %>%
      rowwise() %>%
      mutate(sim_id = list(1:n_iter),
             flux = list(rpois(n = n_iter, lambda = flux))) %>%
      unnest(cols = c("sim_id", "flux")) %>%
      relocate(sim_id, n_iter, .before = flux)
  } else if (model_type == "fcm") {
    next # No need to do anything if you want the non-stochastic variant
  } else (
    stop("Error: invalid model type specified.")
  )

  # Apply the flux collision model to each row in the model input dataframe
  model_output <- model_input %>%
    rowwise() %>%
    mutate(mortality = fcm(flux = flux,
                           a_macro = a_macro,
                           h_prop = h_prop,
                           h_prop_ref = h_prop_ref,
                           rotor_d = rotor_d,
                           rotor_d_ref = rotor_d_ref,
                           turb_dist = turb_dist,
                           turb_dist_ref = turb_dist_ref,
                           turbs_e = turbs_e,
                           turbs_e_ref = turbs_e_ref,
                           p_col = p_col))

  return(model_output)
}

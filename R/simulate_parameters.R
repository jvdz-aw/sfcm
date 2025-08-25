#' Simulate parameters
#'
#' @description
#' Simulates new values of specified parameters by randomly sampling values from user-specified 
#' probability distributions. The function uses a dataframe of parameter values 
#' (`simulation_input`), a vector of parameter names to simulate (`parameters`), a vector of 
#' probability distributions to apply (`distributions`), and the number of samples to 
#' generate (`n`), returning a simulated `model_input` object that can be supplied to [run_model()].
#' 
#' Simulation is currently only supported for the parameters `flux`, `turbs_e`, and `p_col`. 
#' The normal, Poisson, negative binomial, and beta distributions are implemented, 
#' but not all distributions are available for every parameter. See the \strong{Parameters} 
#' section for details on which distributions can be used with each parameter.
#' 
#' @details
#' When multiple parameters are simulated it is recommended to generate a large enough number of random
#' samples (e.g., \emph{n} > 10000) in order to prevent correlation among simulated parameters.
#' 
#' @param simulation_input A dataframe containing input parameters for the simulation. Parameters to 
#' simulate should have the following column names: `<par>_mean` and `<par>_sd`, where `<par>` is the name
#' of the parameter to simulate. 
#' @param parameters A string vector of parameters to simulate. Valid options are: `flux`, `turbs_e` or `p_col`.
#' @param distributions A string vector of probability distributions to use for generating random samples of a 
#' parameter. Valid options are: `normal`, `poisson`, `nbinom` and `beta`.
#' @param n The number of random samples to generate.
#'
#' @returns A `model_input` object with \emph{n} rows containing simulated parameters.
#' 
#' @examples
#' df <- data.frame(
#'   species = c("A", "B"),
#'   flux_mean = c(50, 100),
#'   flux_sd = c(10, 50),
#'   a_macro = 0.95,
#'   f_prop = 1,
#'   h_prop = 0.6,
#'   h_prop_ref = 0.67,
#'   rotor_d = 170,
#'   rotor_d_ref = 60,
#'   turb_dist = 600,
#'   turb_dist_ref = 250,
#'   turbs_e_mean = c(4, 2),
#'   turbs_e_sd = 0,
#'   turbs_e_ref = 4,
#'   p_col_mean = c(0.5, 0.8),
#'   p_col_sd = c(0.05, 0.1)
#' )
#' 
#' pars <- c("flux", "turbs_e", "p_col")
#' dists <- c("nbinom", "poisson", "beta")
#' 
#' sim_out <- simulate_parameters(df, parameters = pars, distributions = dists, n = 10)
#' class(sim_out)
#' 
#' res <- run_model(sim_out)
#' 
#' @seealso [model_input()], [run_model()]
#' 
#' @importFrom purrr reduce
#' @importFrom tidyr unnest
#' @importFrom magrittr `%>%`
#' @importFrom dplyr select rename_with relocate
#' @importFrom stringr str_remove
#' @importFrom tidyselect all_of any_of last_col
#' @importFrom stats setNames
#' 
#' @export
simulate_parameters <- function(simulation_input, parameters, distributions, n = 1000) {

  # Check dataframe type
  if (!is_valid_dataframe(simulation_input)) {
    stop("Simulation input is not a dataframe or tibble.")
  }

  # Check for NAs in columns
  na_cols <- check_na_cols(simulation_input)
  if (any(na_cols)) {
    stop(paste("The following columns in simulation input contain NAs:", names(simulation_input)[na_cols]))
  }

  # Parameter and distributions vectors should have equal lengths, if not one of them is not set correctly
  if (length(parameters) != length(distributions)) {
    stop("Parameters and distributions vectors have different lengths.")
  }

  # Map parameter names to distributions by converting to named vector
  dists <- setNames(distributions, parameters)

  # Define which distributions are allowed for parameters that can be simulated
  allowed_distributions <- list(
    "flux" = c("normal", "poisson", "nbinom"),
    "turbs_e" = c("poisson"),
    "p_col" = c("beta")
  ) 

  # Define output column order
  col_order <- c(
    "flux", "a_macro", "f_prop", "h_prop", "h_prop_ref", "rotor_d", "rotor_d_ref", 
    "turb_dist", "turb_dist_ref", "turbs_e", "turbs_e_ref", "p_col"
  )

  sample_methods <- get_sample_methods() # Get sampling dispatch
  cols_to_remove_rename <- get_cols_remove_rename(parameters)   # Determine columns to remove/rename

  # Perform checks on parameters
  for (parameter in parameters) {

    if (!parameter %in% names(allowed_distributions)) {
      stop(paste("The following parameter cannot be simulated:", parameter))
    }

    selected_distribution <- dists[[parameter]]
    if (!selected_distribution %in% allowed_distributions[[parameter]]) {
      stop(paste0("Probability distribution '", selected_distribution, "' is unavailable for parameter '", parameter, "'"))
    }
  }

  # Add simulation IDs per row
  simulation_input <- simulation_input %>%
    mutate(simulation_id = map(seq_len(nrow(simulation_input)), ~ seq_len(n)))

  # Simulate values using reduce instead of a for-loop for efficiency
  simulation_output <- reduce(parameters, function(df, parameter) {

    # Retrieve distribution selected for parameter and insert into dispatch to get the relevant sampling function
    dist_name <- dists[[parameter]]
    dist_func <- sample_methods[[dist_name]]

    # Generate random samples
    samples <- dist_func(df, parameter, n)

    # Add generated samples to a new column named `<parameter>_samples`
    sample_col <- paste0(parameter, "_samples")
    df[[sample_col]] <- samples
    df
  }, .init = simulation_input) %>%
    unnest(cols = c("simulation_id", all_of(paste0(parameters, "_samples")))) %>% # Unnest simulation_id and all parameter columns
    select(!any_of(cols_to_remove_rename$to_remove)) %>%
    rename_with(~str_remove(.x, "_mean|_samples"), any_of(cols_to_remove_rename$to_rename)) %>%
    relocate(all_of(col_order), .after = last_col())

  model_input(simulation_output) # Converts simulation output to `model_input` object
}
#' Simulate parameters
#'
#' @description
#' Generate random samples of a parameter using one of the available sampling functions.
#'
#' @param simulation_input A dataframe containing input parameters for the simulation.
#' @param parameters A vector of parameters to simulate.
#' @param distributions A vector of distributions to use for generating random samples of a parameter.
#' @param n The number of random samples to generate.
#'
#' @returns A `model_input` object with \emph{n} rows containing simulated parameters.
#' @importFrom purrr reduce
#' @importFrom tidyr unnest
#' @importFrom magrittr `%>%`
#' @importFrom dplyr select rename_with relocate
#' @importFrom stringr str_remove
#' @importFrom tidyselect all_of any_of last_col
#' @export
simulate_parameters <- function(simulation_input, parameters, distributions, n = 1000) {

  # Check whether the input dataframe is not a grouped or rowwise tibble
  validate_simulation_input(simulation_input)

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
    unnest(cols = c(simulation_id, all_of(paste0(parameters, "_samples")))) %>% # Unnest simulation_id and all parameter columns
    select(!any_of(cols_to_remove_rename$to_remove)) %>%
    rename_with(~str_remove(.x, "_mean|_samples"), any_of(cols_to_remove_rename$to_rename)) %>%
    relocate(all_of(col_order), .after = last_col())

  model_input(simulation_output) # Converts simulation output to `model_input` object
}
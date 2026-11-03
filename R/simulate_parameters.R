#' Simulate parameters
#'
#' @description
#' Simulates new values of specified parameters by randomly sampling values from user-specified 
#' probability distributions. The function uses a dataframe of parameter values 
#' (`simulation_input`), a vector of parameter names to simulate (`parameters`), a vector of 
#' probability distributions to apply (`distributions`), and the number of samples to 
#' generate (`n`), returning a simulated `model_input` object that can be supplied to [run_model()].
#' 
#' We currently recommend to only simulate `flux` values. Simulating `p_col_ref` values
#' is considered \strong{EXPERIMENTAL} at this stage; it is available for exploratory analysis
#' but should \emph{not} be used in formal studies.
#' 
#' The normal, Poisson, negative binomial, and beta distributions are implemented, 
#' but not all distributions are available for every parameter. See the \strong{Parameters} 
#' section for details on which distributions can be used with each parameter.
#' 
#' @details
#' When multiple parameters are simulated it is recommended to generate a large enough number of random
#' samples (e.g., \emph{n} > 10000) in order to prevent correlation among simulated parameters.
#' 
#' @param simulation_input A dataframe containing input parameters. For each parameter in
#' `parameters`, the dataframe must contain columns `<par>_mean` and `<par>_sd` where
#' `<par>` is the parameter name. Other columns (e.g., `a_macro`, `f_prop`, etc.) are
#' passed through to the output.
#' @param parameters A character vector of parameters to simulate. Valid options are: `flux` or
#' `p_col_ref` (\strong{EXPERIMENTAL}).
#' @param distributions A character vector of probability distributions to use for generating random samples of a 
#' parameter, one for each parameter in `parameters`. Must have the same length as `parameters`. Valid options are: `normal`, `poisson`, `nbinom` and `beta`.
#' @param n The number of random samples to generate per row. For simulating multiple parameters, we recommend n > 10000 
#' to prevent correlation among parameters. 
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
#'   turbs_e = c(4, 2),
#'   turbs_e_ref = 4,
#'   p_col_ref_mean = c(0.5, 0.8),
#'   p_col_ref_sd = c(0.05, 0.1)
#' )
#' 
#' pars <- c("flux", "p_col_ref")
#' dists <- c("nbinom", "beta")
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
#' @importFrom dplyr mutate select rename rename_with relocate
#' @importFrom stringr str_remove
#' @importFrom tidyselect all_of any_of last_col
#' @importFrom stats setNames
#' @importFrom tibble as_tibble
#' 
#' @export
simulate_parameters <- function(simulation_input, parameters, distributions, n = 1000) {

  # Check dataframe type
  if (!is_valid_dataframe(simulation_input)) {
    stop("Simulation input is not a dataframe or tibble.", call. = FALSE)
  }

  # Check for NAs in columns
  na_cols <- check_na_cols(simulation_input)
  if (any(na_cols)) {
    stop(paste("The following columns in simulation input contain NAs:", names(simulation_input)[na_cols]), call. = FALSE)
  }

  # Parameter and distributions vectors should have equal lengths, if not one of them is not set correctly
  if (length(parameters) != length(distributions)) {
    stop("Parameters and distributions vectors have different lengths.", call. = FALSE)
  }

  # Map parameter names to distributions by converting to named vector
  dists <- setNames(distributions, parameters)

  # Define which distributions are allowed for parameters that can be simulated
  allowed_distributions <- list(
    "flux" = c("normal", "poisson", "nbinom"),
    "p_col_ref" = c("beta")
  )

  # Define output column order
  col_order <- c(
    "flux", "a_macro", "f_prop", "h_prop", "h_prop_ref", "rotor_d", "rotor_d_ref", 
    "turb_dist", "turb_dist_ref", "turbs_e", "turbs_e_ref", "p_col_ref"
  )

  # Mark experimental parameters
  experimental_pars <- "p_col_ref"

  sample_methods <- get_sample_methods() # Get sampling dispatch

  # Perform checks on parameters
  for (parameter in parameters) {

    if (parameter %in% experimental_pars) {
      warning(paste0("Simulating values for '", parameter, "' is experimental and should not be used in formal studies."), call. = FALSE)
    }

    if (!parameter %in% names(allowed_distributions)) {
      stop(paste("The following parameter cannot be simulated:", parameter), call. = FALSE)
    }

    # Check whether required columns exist
    req_cols <- c(paste0(parameter, "_mean"), paste0(parameter, "_sd"))
    missing_cols <- req_cols[!req_cols %in% names(simulation_input)]
    if (length(missing_cols) > 0) {
      stop(paste0("Missing required columns for parameter ", parameter, ": ", paste(missing_cols, collapse = ", ")), call. = FALSE)
    }

    # Check whether distribution is valid for parameter
    selected_distribution <- dists[[parameter]]
    if (!selected_distribution %in% allowed_distributions[[parameter]]) {
      stop(
        sprintf(
          "Probability distribution '%s' is unavailable for parameter '%s'. Available options: '%s'.",
          selected_distribution, parameter, paste(allowed_distributions[[parameter]], collapse = "', '")
        ),
        call. = FALSE)
    }
  }

  # Check whether n is a positive integer
  if (!(is.numeric(n)) || length(n) != 1 || n <= 0 || n %% 1 != 0) {
    stop("'n' must be a single positive integer.", call. = FALSE)
  }

  simulatable <- names(allowed_distributions)
  unsimulated <- setdiff(simulatable, parameters)
  rename_map <- setNames(paste0(unsimulated, "_mean"), unsimulated)

  # Promote <par>_mean to <par> for unsimulated params; drop all _mean/_sd helpers
  clean_input <- simulation_input %>%
    rename(any_of(rename_map)) %>%
    select(!any_of(c(paste0(simulatable, "_mean"), paste0(simulatable, "_sd"))))
  
  # Expand rows and tag with simulation id
  simulation_output <- as_tibble(clean_input[rep(seq_len(nrow(clean_input)), each = n), ])
  simulation_output$simulation_id <- rep(seq_len(n), times = nrow(clean_input))

  # Draw samples from the original input and store in the bare parameter column
  for (parameter in parameters) {
    samples <- unlist(sample_methods[[dists[[parameter]]]](simulation_input, parameter, n))
    simulation_output[[parameter]] <- samples
  }
  
  existing_cols <- intersect(col_order, names(simulation_output))
  simulation_output <- relocate(simulation_output, all_of(existing_cols), .after = last_col())

  model_input(simulation_output) # Converts simulation output to `model_input` object
}
#' Sample from a normal distribution
#'
#' Internal function to generate normal samples using parameters from a dataframe.
#'
#' @param df A dataframe containing columns `<param>_mean` and `<param>_sd`.
#' @param param The base name of the parameter (e.g., "flux")
#' @param n Number of samples to draw per row.
#'
#' @returns A list-column of numeric vectors of length `n`.
#' @keywords internal
#' @importFrom purrr map2
sample_norm <- function(df, param, n) {
  mu <- df[[paste0(param, "_mean")]]
  sigma <- df[[paste0(param, "_sd")]]
  map2(mu, sigma, ~rnorm(n, mean = .x, sd = .y))
}


#' Sample from Poisson distribution
#'
#' Internal function to generate Poisson samples using parameters from a dataframe.
#'
#' @param df A dataframe containing column `<param>_mean` (`<param>_sd` is ignored).
#' @param param The base name of the parameter (e.g., "flux")
#' @param n Number of samples to draw per row.
#'
#' @returns A list-column of numeric vectors of length `n`.
#' @keywords internal
#' @importFrom purrr map
sample_poisson <- function(df, param, n) {
  lambda <- df[[paste0(param, "_mean")]]
  map(lambda, ~rpois(n, lambda = .x))
}


#' Sample from beta distribution
#'
#' Internal function to generate beta samples using parameters from a dataframe. Shape
#' parameters are estimated from the mean and standard deviation.
#'
#' @param df A dataframe containing columns `<param>_mean` and `<param>_sd`.
#' @param param The base name of the parameter (e.g., "flux")
#' @param n Number of samples to draw per row.
#'
#' @returns A vector of numbers of length `n`.
#' @keywords internal
#' @importFrom purrr map2
sample_beta <- function(df, param, n) {
  mu <- df[[paste0(param, "_mean")]]
  sigma <- df[[paste0(param, "_sd")]]
  shape1 <- mu * ((mu * (1 - mu) / sigma^2) - 1)
  shape2 <- ((1 - mu) * (mu * (1 - mu) / sigma^2) - 1)
  map2(shape1, shape2, ~rbeta(n, shape1 = .x, shape2 = .y))
}


#' Sample from negative binomial distribution
#'
#' Internal function to generate negative binomial samples using parameters from
#' a dataframe. The overdispersion parameter is estimated from the mean and standard deviation.

#' @param df A dataframe containing columns `<param>_mean` and `<param>_sd`.
#' @param param The base name of the parameter (e.g., "flux")
#' @param n Number of samples to draw per row.
#'
#' @returns A vector of numbers of length `n`.
#' @keywords internal
#' @importFrom purrr map2
sample_nbinom <- function(df, param, n) {
  mu <- df[[paste0(param, "_mean")]]
  sigma <- df[[paste0(param, "_sd")]]
  k <- mu^2 / (sigma^2 - mu)
  map2(k, mu, ~rnbinom(n, size = .x, mu = .y))
}


#' Get sampling dispatch
#'
#' This internal function returns a named list mapping available sampling method
#' keywords to the sampling functions.
#'
#' @returns A named list mapping sampling method keywords to functions.
#' @keywords internal
get_sampling_dispatch <- function() {
  list(
    "normal" = sample_norm,
    "poisson" = sample_poisson,
    "beta" = sample_beta,
    "nbinom" = sample_nbinom
  )
}


#' Get allowed distributions
#'
#' This internal function returns a named list mapping parameter keywords to vectors
#' of distributions. This list specifies which distributions are allowed
#' for each parameter that can be sampled.
#'
#' @returns A named list mapping parameter keywords to distributions.
#' @keywords internal
get_allowed_dists <- function() {
  list(
    "flux" = c("normal", "poisson", "nbinom"),
    "turbs_e" = c("poisson"),
    "p_col" = c("beta")
  )
}


#' Simulate parameters
#'
#' @description
#' Generate random samples of a parameter using one of the available sampling functions.
#'
#' @param simulation_input A dataframe containing input parameters for the simulation.
#' @param parameters A vector of parameters to simulate.
#' @param distributions A named list containing mappings of parameters to distributions.
#' @param n The number of random samples to generate.
#'
#' @returns A dataframe with `n` rows containing simulated parameters.
#' @importFrom purrr reduce
#' @importFrom tidyr unnest
#' @importFrom magrittr `%>%`
#' @importFrom dplyr select rename_with relocate
#' @importFrom stringr str_remove
#' @importFrom tidyselect all_of any_of last_col
#' @export
simulate_parameters <- function(simulation_input, parameters, distributions, n = 1000) {

  # Perform checks on data
  if (!is_valid_dataframe(simulation_input)) {
    stop("Simulation input is not a dataframe or tibble.")
  }

  # Check for NAs in columns
  na_cols <- check_na_cols(simulation_input)
  if (any(na_cols)) {
    stop(paste("The following columns in simulation input contain NAs:", names(simulation_input)[na_cols]))
  }

  # Get allowed distributions per parameter
  allowed_distributions <- get_allowed_dists()

  # Get sampling dispatch
  sampling_dispatch <- get_sampling_dispatch()

  # Perform checks on parameters
  for (parameter in parameters) {

    if (!parameter %in% names(distributions)) {
      stop(paste("No distribution specified for parameter:", parameter))
    }

    if (!parameter %in% names(allowed_distributions)) {
      stop(paste("Parameter not recognized:", parameter))
    }

    chosen_distribution <- distributions[[parameter]]
    if (!chosen_distribution %in% allowed_distributions[[parameter]]) {
      stop(paste0("Distribution '", chosen_distribution, "' not allowed for parameter '", parameter, "'"))
    }
  }

  # Add simulation IDs per row
  simulation_input <- simulation_input %>%
    mutate(simulation_id = map(seq_len(nrow(simulation_input)), ~ seq_len(n)))

  # Simulate values using reduce instead of a for-loop for efficiency
  simulation_input <- reduce(parameters, function(df, parameter) {

    # Retrieve distribution selected for parameter and insert into dispatch to get the relevant sampling function
    dist_name <- distributions[[parameter]]
    dist_func <- sampling_dispatch[[dist_name]]

    # Generate random samples
    samples <- dist_func(df, parameter, n)

    # Add generated samples to a new column named `<parameter>_samples`
    sample_col <- paste0(parameter, "_samples")
    df[[sample_col]] <- samples
    df
  }, .init = simulation_input)

  # Define columns that need to be renamed or removed
  pars_allowed <- names(allowed_distributions)
  pars_not_to_sim <- pars_allowed[!pars_allowed %in% parameters]
  cols_to_rename <- c(paste0(pars_not_to_sim, "_mean"), paste0(parameters, "_samples"))
  cols_to_remove <- c(paste0(pars_allowed, "_sd"), paste0(parameters, "_mean"))

  # Define a column order based upon the model input data specification
  data_spec <- model_input_data_spec()
  col_order <- names(data_spec)

  # Unnest simulation_id and all parameter columns, then perform some renaming and clean up
  simulation_input %>%
    unnest(cols = c(simulation_id, all_of(paste0(parameters, "_samples")))) %>%
    select(!any_of(cols_to_remove)) %>%
    rename_with(~str_remove(.x, "_mean|_samples"), any_of(cols_to_rename)) %>%
    relocate(all_of(col_order), .after = last_col())
}


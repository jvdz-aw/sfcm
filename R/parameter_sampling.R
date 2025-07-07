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
#' @param data A dataframe or tibble containing input parameters for simulation.
#' @param parameters A vector of parameters to simulate.
#' @param distributions A named list containing mappings of parameters to distributions.
#' @param n The number of random samples to generate.
#'
#' @returns A dataframe with `n` rows containing simulated parameters.
#' @importFrom purrr reduce
#' @importFrom tidyr unnest matches
#' @importFrom dplyr select
#' @importFrom magrittr `%>%`
#' @export
simulate_parameters <- function(data, parameters, distributions, n = 1000) {

  # Get allowed distributions per parameter
  allowed_distributions <- get_allowed_dists()

  # Get sampling dispatch
  sampling_dispatch <- get_sampling_dispatch()

  # Peform checks on parameters
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
  data <- data %>%
    mutate(simulation_id = map(seq_len(nrow(data)), ~ seq_len(n)))

  # Simulate values
  data <- reduce(parameters, function(df, parameter) {

    # Retrieve distribution selected for parameter and insert into dispatch to get the relevant sampling function
    dist_name <- distributions[[parameter]]
    dist_func <- sampling_dispatch[[dist_name]]

    # Generate random samples
    samples <- dist_func(df, parameter, n)
    df[[parameter]] <- samples
    df
  }, .init = data)

  # Unnest simulation_id and all parameter columns, then remove columns with mean and sd
  data %>%
    unnest(cols = c(simulation_id, all_of(parameters))) %>%
    select(!matches("_mean|_sd"))
}


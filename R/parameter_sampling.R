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


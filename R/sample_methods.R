#' Sample from normal distribution
#'
#' @description
#' Internal function for generating random normal samples using parameters from a dataframe.
#'
#' @param df A dataframe containing columns `<param>_mean` and `<param>_sd`.
#' @param param The base name of the parameter (e.g., "flux").
#' @param n Number of samples to draw per row.
#'
#' @returns A list-column of numeric vectors of length `n`.
#' 
#' @keywords internal
#' 
#' @importFrom purrr map2
sample_norm <- function(df, param, n) {
  mu <- df[[paste0(param, "_mean")]]
  sigma <- df[[paste0(param, "_sd")]]
  map2(mu, sigma, ~rnorm(n, mean = .x, sd = .y))
}


#' Sample from Poisson distribution
#'
#' @description
#' Internal function for generating random Poisson samples using parameters from a dataframe.
#'
#' @param df A dataframe containing column `<param>_mean` (`<param>_sd` is ignored).
#' @param param The base name of the parameter (e.g., "flux").
#' @param n Number of samples to draw per row.
#'
#' @returns A list-column of numeric vectors of length `n`.
#' 
#' @keywords internal
#' 
#' @importFrom purrr map
sample_poisson <- function(df, param, n) {
  lambda <- df[[paste0(param, "_mean")]]
  map(lambda, ~rpois(n, lambda = .x))
}


#' Estimate beta distribution parameters from mean and standard deviation
#' 
#' @description
#' Internal function that calculates the shape parameters (\eqn{\alpha} and \eqn{\beta}) for a
#' beta distribution based on the method of moments, given a desired mean and standard deviation.
#' 
#' @param mu The desired mean of the distribution. Must be between 0 and 1.
#' @param sigma The desired standard deviation.
#' 
#' @details
#' The beta distribution is defined on the interval [0, 1]. For a valid distribution to 
#' exist, the variance (\eqn{\sigma^2}) must be less than \eqn{\mu(1 - \mu)}. 
#' 
#' @returns A named list containing:
#' \itemize{
#'   \item \code{shape_a}: The first shape parameter (\eqn{\alpha}).
#'   \item \code{shape_b}: The second shape parameter (\eqn{\beta}).
#' }
#'
#' @examples
#' # Get parameters for a distribution centered at 0.7 with small spread
#' get_beta_params(mu = 0.7, sigma = 0.05)
#' 
#' @keywords internal
get_beta_params <- function(mu, sigma) {
  # Calculate variance
  sigma2 <- sigma^2

  # Variance must be less than mean * (1 - mean)
  if (sigma2 >= mu * (1 - mu)) {
    stop("The provided SD is too high for the given mean to form a valid beta distribution.")
  }

  # Calculate shape parameters
  term <- ((mu * (1 - mu) / sigma2) - 1) # Common term
  shape_a <- mu * term
  shape_b <- (1 - mu) * term

  return(list(shape_a = shape_a, shape_b = shape_b))
}


#' Sample from beta distribution
#'
#' @description
#' Internal function for generating random beta samples using parameters from a dataframe. Shape
#' parameters are estimated from the mean and standard deviation.
#'
#' @param df A dataframe containing columns `<param>_mean` and `<param>_sd`.
#' @param param The base name of the parameter (e.g., "flux").
#' @param n Number of samples to draw per row.
#'
#' @returns A vector of numbers of length `n`.
#' 
#' @keywords internal
#' 
#' @importFrom purrr map2
sample_beta <- function(df, param, n) {
  mu <- df[[paste0(param, "_mean")]]
  sigma <- df[[paste0(param, "_sd")]]

  map2(mu, sigma, \(x, y) {
    if (y == 0) { # Check for zero to prevent division by zero
      rep(x, n) # Return a vector of n values equal to the mean if SD equals zero
    } else {
      params <- get_beta_params(x, y)
      rbeta(n, shape1 = params$shape_a, shape2 = params$shape_b)
    }
  })
}


#' Sample from negative binomial distribution
#'
#' @description
#' Internal function for generating random negative binomial samples using parameters from
#' a dataframe. The overdispersion parameter is estimated from the mean and standard deviation.
#'
#' @param df A dataframe containing columns `<param>_mean` and `<param>_sd`.
#' @param param The base name of the parameter (e.g., "flux").
#' @param n Number of samples to draw per row.
#'
#' @returns A vector of numbers of length `n`.
#' 
#' @keywords internal
#' 
#' @importFrom purrr map2
sample_nbinom <- function(df, param, n) {
  mu <- df[[paste0(param, "_mean")]]
  sigma <- df[[paste0(param, "_sd")]]
  k <- mu^2 / (sigma^2 - mu)
  map2(k, mu, ~rnbinom(n, size = .x, mu = .y))
}


#' Get sampling methods
#'
#' @description
#' This internal function returns a named list mapping available sampling method
#' keywords to the sampling functions.
#'
#' @returns A named list mapping sampling method keywords to functions.
#' 
#' @keywords internal
get_sample_methods <- function() {
  list(
    "normal" = sample_norm,
    "poisson" = sample_poisson,
    "beta" = sample_beta,
    "nbinom" = sample_nbinom
  )
}
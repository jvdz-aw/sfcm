#' Draw parameters
#'
#' @description
#' Draw parameters using three draw methods. Currently, the supported options are
#' `PointEstimate`, `Bootstrap` and `TruncNorm`. The `PointEstimate` option replicates
#' a single point estimate `n_iterations` times, while the `Bootstrap` option bootstraps
#' a parameter vector with replacement `n_iterations` times. The `TruncNorm` option samples
#' parameter values from a truncated normal distribution `n_iterations` using a `mean` and `sd`.
#' By default, the `TruncNorm` option assumes infinite lower and upper bounds, but these
#' can be specified by the user.
#' @param par_name
#' @param draw_method
#' @param par_vec
#' @param n_iterations
#' @param mean
#' @param sd
#' @param lower_bound
#' @param upper_bound
#' @export
draw_parameters <- function(par_name,
                            draw_method,
                            par_vec = NULL,
                            n_iterations,
                            mean = NULL,
                            sd = NULL,
                            lower_bound = -Inf,
                            upper_bound = Inf) {


  # Check which method to use
  if (draw_method == "PointEstimate") {

    # Check whether a single parameter value was supplied
    if (!is.null(par_vec) && !length(par_vec) > 1) {
      par_draws <- rep(par_vec, times = n_iterations)
    } else {
      stop("Error: `PointEstimate` requires a single parameter value.")
    }

  } else if (draw_method == "Bootstrap") {

    # Check whether a vector of parameter values was supplied
    if (!is.null(par_vec) && !length(par_vec) <= 1) {
      par_draws <- sample(par_vec, size = n_iterations, replace = TRUE)
    } else {
      stop("Error: `Bootstrap` requires a vector of parameter values.")
    }

  } else if (draw_method == "TruncNorm") {

    # Check if mean and sd are specified
    if (!is.null(mean) && !is.null(sd)) {
      par_draws <- truncnorm::rtruncnorm(n = n_iterations,
                                         mean = mean,
                                         sd = sd,
                                         a = lower_bound,
                                         b = upper_bound)

    } else {
      stop("Error: `TruncNorm` requires values for mean and SD.")
    }
  } else {
    stop("Error: invalid method specified. Valid options are `PointEstimate`, `Bootstrap` or `TruncNorm`.")
  }

  # Simulation IDs will be padded accordingly based upon the number of iterations to prevent odd sorting results
  sim_ids <- factor(paste0("sim_", sprintf(paste0("%0", nchar(as.character(n_iterations)), "d"), 1:n_iterations)))

  # Create output dataframe
  output <- data.frame(sim_id = sim_ids)
  output[[par_name]] <- par_draws
  return(output)
}

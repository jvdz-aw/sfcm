# sfcm 0.2.0

## New features

* Added an internal generic input validation function `validate_sample_method_input()` for validating input passed to random sampling wrapper functions in `sample_methods.R` (#5).

* Added distribution-specific guard clauses to `sample_norm()`, `sample_poisson()`, `sample_beta()` and `sample_nbinom()` (#5).

* Added an internal helper function `get_beta_params()` that estimates Beta distribution parameters from a `mean` and `sd`, and checks whether allowed values were provided (#5). 

## Bug fixes

* `sample_beta()` now properly handles a standard deviation of zero, which causes the function to output the mean (#3). This is similar to how `rnorm()` handles this

* `sample_beta()` now calls the new helper function `get_beta_params()` to perform the Beta distribution parameter estimation, which correctly estimates the `shape2` parameter supplied to `rbeta()` (#4).

## Possible breaking changes

* Enhanced input validation has been implemented to ensure data integrity. Users with existing scripts may encounter new errors if those scripts previously passed invalid or inconsistent values that were silently accepted in earlier versions.

# sfcm 0.1.0

* Initial release.

* Provides tools for assessing bird mortality in planned wind farms using Flux Collision Models.

* Implements a stochastic Flux Collision Model to account for parameter uncertainty.

* Supports simulation of flux, number of turbines encountered and collision probability using Normal, Poisson, Negative Binomial and Beta distributions.

* Provides implicit data validation through the 'model_input' S3 class.
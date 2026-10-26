# sfcm 0.3.0

## New features

* Added tutorial vignettes that describe how to run the deterministic and stochastic flux collision models (#14).

* `simulate_parameters()` now prints a warning when simulating values for experimental parameters, which currently applies to `p_col_ref` (#11).

* Added input validation to `simulate_parameters()`: required `<parameter>_mean` and `<parameter>_sd` columns are now checked before sampling, and `n` must be a single positive integer (#15).

* Optimized `simulation_id` creation and sampling logic in `simulate_parameters()` (#15).

## Bug fixes

* `simulate_parameters()` now only uses columns that exist in the simulation output when ordering columns (#15).

* Improved the error message thrown when a probability distribution is unavailable for a parameter (#15).

* Added a missing `mutate` import from dplyr (#15).

## Other changes

* Added a standard R CMD check workflow that runs on pull requests and pushes to the main branch (#12).

* Updated the installation instructions in the README (#13).

* Clarified the documentation of `simulate_parameters()`: the recommendation for `n`, the relationship between `parameters` and `distributions`, and which columns are required in `simulation_input` (#15).

## Possible breaking changes

* `simulate_parameters()` no longer supports simulating values for the `turbs_e` parameter (#17). Simulation input data frames should provide `turbs_e` as a regular column (as in `model_input` data frames) instead of `turbs_e_mean` and `turbs_e_sd` columns.

* The `p_col` parameter has been renamed to `p_col_ref` (#16). Scripts and data frames using `p_col` should use `p_col_ref` instead.

* `f_prop` is now a required argument in `fcm()` (#16).

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
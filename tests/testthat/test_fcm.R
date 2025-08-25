library(testthat)

test_that("Test a single mortality calculation using FCM", {

  # Parameter estimates obtained from empirical data
  flux <- 17236.1111
  a_macro <- 0.6
  f_prop <- 1
  h_prop <- 0.46961326
  h_prop_ref <- 0.67
  rotor_d <- 170
  rotor_d_ref <- 60
  turb_dist <- 628.6667
  turb_dist_ref <- 250
  turbs_e <- 2
  turbs_e_ref <- 4.242641
  p_col <- 0.0017

  mortality <- fcm(flux = flux,
                   a_macro = a_macro,
                   h_prop = h_prop,
                   h_prop_ref = h_prop_ref,
                   rotor_d = rotor_d,
                   rotor_d_ref = rotor_d_ref,
                   turb_dist = turb_dist,
                   turb_dist_ref = turb_dist_ref,
                   turbs_e = turbs_e,
                   turbs_e_ref = turbs_e_ref,
                   p_col = p_col)

  # Expected mortality given the empirical estimates of parameters
  exp_mort <- 2.553196
  expect_equal(mortality, exp_mort, tolerance = 0.001)
})


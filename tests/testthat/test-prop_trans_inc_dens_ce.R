test_that("prop_trans_inc_dens_ce works as expected", {
  set.seed(23)
  df <- generate_data(
    n = 100,
    cens = c(0, 0.2),
    haz_ae = 2.3,
    haz_death = 5.3,
    haz_soft = 1.2
  )
  result <- prop_trans_inc_dens_ce(data = df, ce = 2, tau = 0.1)
  expected <- c(ae_prob = 0.296148137, ae_prob_var = 0.002755189)
  expect_equal(result, expected, tolerance = 1e-4)
})

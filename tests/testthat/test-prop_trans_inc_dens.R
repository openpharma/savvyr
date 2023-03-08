test_that("prop_trans_inc_dens works as expected", {
  set.seed(23)
  df <- generate_data(
    n = 10,
    cens = c(0, 0.2),
    haz_ae = 4.3,
    haz_death = 5.3,
    haz_soft = 1.2
  )
  result <- prop_trans_inc_dens(data = df, tau = 0.1)
  expected <- c(ae_prob = 0.33325, ae_prob_var = 0.0365)
  expect_equal(result, expected, tolerance = 1e-4)
})

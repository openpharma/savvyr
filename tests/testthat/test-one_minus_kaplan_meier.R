test_that("one_minus_kaplan_meier works as expected", {
  set.seed(23)
  df <- generate_data(
    n = 25,
    cens = c(0.2, 3),
    haz_ae = 0.2,
    haz_death = 0.3,
    haz_soft = 0.5
  )
  result <- one_minus_kaplan_meier(data = df, tau = 4)
  expected <- c(ae_prob =  0.54147805, ae_prob_var = 0.02275271)
  expect_equal(result, expected, tolerance = 1e-4)
})



test_that("prop_trans_inc_prop works as expected", {
  set.seed(123)
  df <- generate_data(
    n = 10,
    cens = c(0, 0.5),
    haz_ae = 4.3,
    haz_death = 5.3,
    haz_soft = 1.2
  )
  result <- prop_trans_inc_dens(data = df, tau = 4)
  expect_vector(result, size = 2)
  expect_named(result, c("ae_prob", "ae_prob_var"))
})

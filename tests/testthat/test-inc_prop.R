test_that("inc_prop works as expected", {
  set.seed(123)
  result <- generate_data(
    n = 10,
    cens = c(0, 0.5),
    haz_ae = 4.3,
    haz_death = 5.3,
    haz_soft = 1.2
  )
  ip <- inc_prop(data = result, tau = 4)
  expect_vector(ip, size = 2)
  expect_named(ip, c("ae_prob", "ae_prob_var"))
})

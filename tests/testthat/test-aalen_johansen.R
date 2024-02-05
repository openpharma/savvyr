test_that("Aalen Johansen works as expected", {
  set.seed(23)
  df <- generate_data(
    n = 25,
    cens = c(0.2, 3),
    haz_ae = 0.2,
    haz_death = 0.3,
    haz_soft = 0.5
  )
  result <- aalen_johansen(data = df, ce = 2, tau = 4)
  expected <- c(ae_prob = 0.2719, ae_prob_var = 0.0119, ce_prob = 0.7281, ce_prob_var = 0.0119)
  expect_equal(result, expected, tolerance = 1e-4)
})

test_that("Aalen Johansen works without events", {
  set.seed(23)
  df <- generate_data(
    n = 25,
    cens = c(0.2, 3),
    haz_ae = 0.2,
    haz_death = 0.3,
    haz_soft = 0.5
  )
  df <- df[df$type_of_event != 1, ]
  result <- aalen_johansen(data = df, ce = 2, tau = 4)
  expected <- c(ae_prob = 0, ae_prob_var = 0, ce_prob = 1, ce_prob_var = 0)
  expect_equal(result, expected, tolerance = 1e-4)
})

test_that("Aalen Johansen works without competing events", {
  set.seed(23)
  df <- generate_data(
    n = 25,
    cens = c(0.2, 3),
    haz_ae = 0.2,
    haz_death = 0.3,
    haz_soft = 0.5
  )
  df <- df[df$type_of_event != 2, ]
  df <- df[df$type_of_event != 3, ]
  result <- aalen_johansen(data = df, ce = 2, tau = 4)
  expected <- c(ae_prob = 0.5897, ae_prob_var = 0.0404, ce_prob = 0, ce_prob_var = 0)
  expect_equal(result, expected, tolerance = 1e-4)
})

test_that("Aalen Johansen works without any events", {
  set.seed(23)
  df <- generate_data(
    n = 25,
    cens = c(0.2, 3),
    haz_ae = 0.2,
    haz_death = 0.3,
    haz_soft = 0.5
  )
  df <- df[df$type_of_event != 1, ]
  df <- df[df$type_of_event != 2, ]
  df <- df[df$type_of_event != 3, ]
  result <- aalen_johansen(data = df, ce = 2, tau = 4)
  expected <- c(ae_prob = 0, ae_prob_var = 0, ce_prob = 0, ce_prob_var = 0)
  expect_equal(result, expected, tolerance = 1e-4)
})

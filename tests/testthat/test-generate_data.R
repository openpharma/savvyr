test_that("generate_data works as expected", {
  set.seed(123)
  result <- generate_data(
    n = 10,
    cens = c(0, 0.5),
    haz_ae = 4.3,
    haz_death = 5.3,
    haz_soft = 1.2
  )
  expect_data_frame(result, nrows = 10L, ncols = 4L)
  expect_named(result, c("id", "time_to_event", "type_of_event", "cens"))
  expect_identical(result$id, 1:10)
  expect_integer(result$type_of_event, lower = 0, upper = 3, any.missing = FALSE)
})

test_that("show bug in v1 of generate_data", {
  set.seed(123)
  df <- generate_data(
    n = 10^6,
    cens = c(1000, 1001),
    haz_ae = 0.2,
    haz_death = 0.3,
    haz_soft = 0.5
  )
  result <- inc_prop(data = df, tau = 4)
  expected <- c(ae_prob = 0.2)
  expect_equal(result["ae_prob"], expected, tolerance = 0.1)
})

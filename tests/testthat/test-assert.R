# assert_ae_data ----

test_that("assert_ae_data passes as expected", {
  data <- data.frame(
    time_to_event = c(12.0, 1.2, 0.3),
    type_of_event = c(1L, 2L, 0L)
  )
  result <- expect_silent(assert_ae_data(data))
  expected <- NULL
  expect_identical(result, expected)
})

test_that("assert_ae_data fails as expected", {
  expect_error(
    assert_ae_data(
      data.frame(bla = 0, bli = 1)
    ),
    "'data$time_to_event' failed",
    fixed = TRUE
  )
  expect_error(
    assert_ae_data(
      data.frame(time_to_event = -1.3, bli = 1)
    ),
    "Element 1 is not >= 0",
    fixed = TRUE
  )
  expect_error(
    assert_ae_data(
      data.frame(time_to_event = 0, bli = 1)
    ),
    "'data$type_of_event' failed",
    fixed = TRUE
  )
  expect_error(
    assert_ae_data(
      data.frame(time_to_event = 0, type_of_event = 4L)
    ),
    "Must be a subset of {'0','1','2','3'}",
    fixed = TRUE
  )
})

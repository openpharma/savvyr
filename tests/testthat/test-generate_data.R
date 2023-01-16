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

#' Assertion of Adverse Event Data
#'
#' Custom assertion to check adverse event data sets.
#'
#' @param data `data.frame` to be checked for `time_to_event` and `type_of_event` columns.
#'
#' @return None.
#'
#' @keywords internal
assert_ae_data <- function(data) {
  assert_data_frame(data, any.missing = FALSE, min.rows = 1, min.cols = 2)
  assert_numeric(data$time_to_event, lower = 0, finite = TRUE)
  assert_integerish(data$type_of_event, any.missing = FALSE)
  assert_subset(data$type_of_event, c(0, 1, 2, 3))
  invisible()
}

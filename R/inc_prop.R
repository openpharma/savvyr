#' Computing the Incidence Proportion
#'
#' @typed data: data.frame
#'   with columns including:
#'   - `time_to_event`: Time to the first AE, death or soft competing
#'     event.
#'   - `type_of_event`: 0 for censored, 1 for AE, 2 for death, 3 for soft
#'     competing event.
#' @typed tau: numeric
#'   milestone at which incidence proportion is computed.
#'
#' @typedreturn vector
#'   with the following entries:
#'   - `ae_prob`: Estimated probability of AE.
#'   - `ae_prob_var`: Variance of that estimate.
#'
#' @export
#' @examples
#' set.seed(123)
#' dat <- generate_data(
#'   n = 5,
#'   cens = c(2, 5),
#'   haz_ae = 2,
#'   haz_death = 3,
#'   haz_soft = 5
#' )
#'
#' inc_prop(dat, tau = 4)
#'
inc_prop <- function(data,
                     tau) {
  assert_data_frame(data, any.missing = FALSE, min.rows = 1, min.cols = 2)
  assert_numeric(data$time_to_event, lower = 0, finite = TRUE)
  assert_integerish(data$type_of_event, any.missing = FALSE)
  assert_subset(data$type_of_event, c(0, 1, 2, 3))
  assert_number(tau, finite = TRUE)
  assert_true(tau > 0)

  n <- nrow(data)
  ae <- nrow(data["type_of_event" == 1 & "time_to_event" <= tau]) / n
  ae_prob_var <- ae * (1 - ae) / n

  c("ae_prob" = ae, "ae_prob_var" = ae_prob_var)
}

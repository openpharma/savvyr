#' Computing the Probability Transform Incidence Density
#'
#' @typed data: data.frame
#'  with columns including
#'  - `time_to_event`: Time to the first AE, death or soft competing event.
#'  - `type_of_event`: 0 for censored, 1 for AE, 2 for death, 3 for soft competing event.
#'
#' @typed tau: number
#'  milestone at which Probability Transform Incidence Density is computed.
#'
#' @typedreturn vector
#'  with the following entries:
#'
#' - `ae_prob`: Estimated probability of AE.
#' - `ae_prob_var`: Variance of that estimate.
#'
#' @export
#'
#' @examples
#' set.seed(123)
#' dat <- generate_data(n = 5, cens = c(2, 5), haz_ae = 2, haz_death = 3, haz_soft = 5)
#' inc_prop(dat, tau = 4)
prop_trans_inc_dens <- function(data,
                                tau) {
  assert_data_frame(data, any.missing = FALSE, min.rows = 1, min.cols = 2)
  assert_numeric(data$time_to_event, lower = 0, finite = TRUE)
  assert_integerish(data$type_of_event, any.missing = FALSE)
  assert_subset(data$type_of_event, c(0, 1, 2, 3))
  assert_number(tau, finite = TRUE)
  assert_true(tau > 0)

  time <- data$time_to_event
  inc_dens <-
    nrow(data[type_of_event == 1 & time_to_event <= tau]) /
      sum(ifelse(time <= tau, time, tau))
  ae_prob <- 1 - exp(-inc_dens * tau)

  var_a_var <-
    nrow(data[type_of_event == 1 & time_to_event <= tau]) /
      sum(ifelse(time <= tau, time, tau))^2
  ae_prob_var <- exp(-inc_dens * tau)^2 * var_a_var * tau^2

  c("ae_prob" = ae_prob, "ae_prob_var" = ae_prob_var)
}

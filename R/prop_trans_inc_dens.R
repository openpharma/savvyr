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
#' prop_trans_inc_dens(dat, tau = 4)
prop_trans_inc_dens <- function(data,
                                tau) {
  assert_ae_data(data)
  assert_number(tau, finite = TRUE)
  assert_true(tau > 0)

  n_ae <- nrow(data[data$type_of_event == 1 & data$time_to_event <= tau, ])
  sum_trunc_times <- sum(ifelse(data$time_to_event <= tau, data$time_to_event, tau))
  inc_dens <- n_ae / sum_trunc_times
  ae_prob <- 1 - exp(-inc_dens * tau)
  var_a_var <- n_ae / sum_trunc_times^2
  ae_prob_var <- exp(-inc_dens * tau)^2 * var_a_var * tau^2

  c("ae_prob" = ae_prob, "ae_prob_var" = ae_prob_var)
}

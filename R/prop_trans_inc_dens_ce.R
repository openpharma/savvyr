#' Probability Transform Incidence Density Accounting for Competing Events
#'
#' This funciton calculates the incidence density of both adverse events and specified competing events
#' observed in [0, tau] and then combines and transforms the incidence densities on a probability scale.
#' Please also refer to formula (4) and (5) in
#'
#' Stegherr, R., Schmoor, C., Beyersmann, J. et al.
#' Survival analysis for AdVerse events with VarYing follow-up times (SAVVY)—
#' estimation of adverse event risks. Trials 22, 420 (2021).
#' https://doi.org/10.1186/s13063-021-05354-x
#'
#' @typed data: data.frame
#'  with columns including
#'  - `time_to_event`: Time to the first AE, death or soft competing event.
#'  - `type_of_event`: 0 for censored, 1 for AE, 2 for death, 3 for soft competing event.
#'
#' @typed tau: number
#'  milestone at which Probability Transform Incidence Density is computed.
#'
#' @typed ce: number
#'  code for competing event.
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
#' prop_trans_inc_dens_ce(dat, ce = 2, tau = 4)
prop_trans_inc_dens_ce <- function(data,
                                   ce,
                                   tau) {
  assert_data_frame(data, any.missing = FALSE, min.rows = 1, min.cols = 2)
  assert_numeric(data$time_to_event, lower = 0, finite = TRUE)
  assert_integerish(data$type_of_event, any.missing = FALSE)
  assert_subset(data$type_of_event, c(0, 1, 2, 3))
  assert_number(tau, finite = TRUE)
  assert_true(tau > 0)
  assert_subset(ce, c(2, 3))


  data$type_of_event2 <- ifelse(ce == 2 & data$type_of_event == 3, 0,
    ifelse(ce == 3 & data$type_of_event == 3, 2, data$type_of_event)
  )

  time2 <- ifelse(data$time_to_event <= tau, data$time_to_event, tau)
  patient_time <- sum(time2)

  incidence_density <- nrow(data[data$type_of_event2 == 1 & data$time_to_event <= tau, ]) / patient_time

  incidence_density_ce <- nrow(data[data$type_of_event2 == 2 & data$time_to_event <= tau, ]) / patient_time

  sum_incidence_densities <- incidence_density + incidence_density_ce
  expected_time_to_event <- exp(-tau * sum_incidence_densities)

  ae_prob <- incidence_density / sum_incidence_densities * (1 - expected_time_to_event)

  var1 <- ((expected_time_to_event * (incidence_density_ce * (1 / expected_time_to_event - 1) + tau * incidence_density * sum_incidence_densities)) / sum_incidence_densities^2)^2 * incidence_density / patient_time
  var2 <- ((expected_time_to_event * incidence_density * (tau * sum_incidence_densities - 1 / expected_time_to_event + 1)) / sum_incidence_densities^2)^2 * incidence_density_ce / patient_time

  ae_prob_var <- var1 + var2

  c("ae_prob" = ae_prob, "ae_prob_var" = ae_prob_var)
}

#' Probability Transform Incidence Density Accounting for Competing Events
#'
#' This function calculates the incidence density of both adverse events and specified competing events
#' observed in `[0, tau]` and then combines and transforms the incidence densities on a probability scale.
#' Please also refer to formulas (4) and (5) in \insertCite{stegherr_survival_2021;textual}{savvyr}.
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
#' @references
#' \insertRef{stegherr_survival_2021}{savvyr}
#'
#' @examples
#' set.seed(123)
#' dat <- generate_data(n = 5, cens = c(2, 5), haz_ae = 2, haz_death = 3, haz_soft = 5)
#' prop_trans_inc_dens_ce(dat, ce = 2, tau = 4)
prop_trans_inc_dens_ce <- function(data,
                                   ce,
                                   tau) {
  assert_ae_data(data)
  assert_subset(ce, c(2, 3))
  assert_number(tau, finite = TRUE)
  assert_true(tau > 0)

  data$type_of_event_accounted <- ifelse(ce == 2 & data$type_of_event == 3, 0,
    ifelse(ce == 3 & data$type_of_event == 3, 2, data$type_of_event)
  )

  time_max_tau <- ifelse(data$time_to_event <= tau, data$time_to_event, tau)
  patient_time <- sum(time_max_tau)

  incidence_density <- nrow(data[data$type_of_event_accounted == 1 & data$time_to_event <= tau, ]) / patient_time

  incidence_density_ce <- nrow(data[data$type_of_event_accounted == 2 & data$time_to_event <= tau, ]) / patient_time

  sum_incidence_densities <- incidence_density + incidence_density_ce
  expected_time_to_event <- exp(-tau * sum_incidence_densities)

  ae_prob <- incidence_density / sum_incidence_densities * (1 - expected_time_to_event)

  var1_part1 <- incidence_density_ce * (1 / expected_time_to_event - 1) +
    tau * incidence_density * sum_incidence_densities
  var1_part2 <- (expected_time_to_event * var1_part1) / sum_incidence_densities^2
  var1 <- var1_part2^2 * incidence_density / patient_time

  var2_part1 <- incidence_density * (tau * sum_incidence_densities - 1 / expected_time_to_event + 1)
  var2_part2 <- (expected_time_to_event * var2_part1) / sum_incidence_densities^2
  var2 <- var2_part2^2 * incidence_density_ce / patient_time

  ae_prob_var <- var1 + var2

  c("ae_prob" = ae_prob, "ae_prob_var" = ae_prob_var)
}

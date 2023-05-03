#' Computing the Probability Transform Incidence Density accounting for competing events
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
#' prop_trans_inc_dens_ce(dat, ce=2, tau = 4)
#'
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
                                ifelse(ce == 3 & data$type_of_event == 3, 2, data$type_of_event))

  time2 <- ifelse(data$time_to_event <= tau, data$time_to_event, tau)
  s2 <- sum(time2)

  id <- nrow(data[data$type_of_event2 == 1 & data$time_to_event <= tau,])/ s2

  id_ce <- nrow(data[data$type_of_event2 == 2 & data$time_to_event <= tau,])/ s2

  tmp <- id + id_ce
  ett <- exp(-tau * tmp)

  ae_prob <- id / tmp * (1 - exp(- tau*tmp))
  ae_prob_var <- (((ett *      (id_ce * (1 / ett - 1) + tau * id * tmp)) / tmp ^ 2) ^ 2 * id / s2 +
                    ((ett * id * (tau * tmp - 1 / ett + 1)) / tmp ^ 2) ^ 2 * id_ce / s2)


  c("ae_prob" = ae_prob, "ae_prob_var" = ae_prob_var)
}



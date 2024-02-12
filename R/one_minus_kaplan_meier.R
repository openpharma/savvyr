#' One Minus Kaplan-Meier
#'
#' This function calculates the one minus Kaplan-Meier estimator of
#' adverse events (while censoring all competing events) observed in `[0, tau]`.
#' Please also refer to formula (4) in \insertCite{stegherr_meta_analytic_2021;textual}{savvyr}.
#'
#' @typed data: data.frame
#'  with columns including
#'  - `time_to_event`: Time to the first AE, death or soft competing event.
#'  - `type_of_event`: 0 for censored, 1 for AE, 2 for death, 3 for soft competing event.
#'
#' @typed tau: number
#'  milestone at which One Minus Kaplan-Meier is computed.
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
#' \insertRef{stegherr_meta_analytic_2021}{savvyr}
#'
#' @examples
#' set.seed(123)
#' dat <- generate_data(n = 5, cens = c(2, 5), haz_ae = 2, haz_death = 3, haz_soft = 5)
#' one_minus_kaplan_meier(dat, tau = 4)
one_minus_kaplan_meier <- function(data,
                                   tau) {
  assert_ae_data(data)
  assert_number(tau, finite = TRUE)
  assert_true(tau > 0)

  n_comp_events <- sum(data$type_of_event == 1)
  if (n_comp_events == 0) {
    ae_prob <- 0
    ae_prob_var <- 0
  } else {
    help <- data.frame(id = data$id)
    help$from <- 0
    help$to <- ifelse(data$type_of_event != 1, "cens", data$type_of_event)
    help$time <- ifelse(data$time_to_event == 0, 0.001, data$time_to_event)

    trans_mat <- matrix(FALSE, 2, 2)
    trans_mat[1, 2] <- TRUE
    state_names <- as.character(0:1)

    etmmm <- etm::etm(help, state_names, trans_mat, "cens", s = 0)
    etm_sum_prob <- summary(etmmm)[[2]]
    n_time_below_tau <- sum(etm_sum_prob$time <= tau)
    etm_sum_prob_selected <- etm_sum_prob[n_time_below_tau, ]
    ae_prob <- etm_sum_prob_selected$P
    ae_prob_var <- etm_sum_prob_selected$var
  }

  c("ae_prob" = ae_prob, "ae_prob_var" = ae_prob_var)
}

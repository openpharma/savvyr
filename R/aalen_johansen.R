#' Aalen Johansen Estimator
#'
#' This function calculates the Aalen Johansen estimator of
#' adverse events  observed in `[0, tau]`.
#' Please also refer to  \insertCite{stegherr_estimating_2021;textual}{savvyr}.
#'
#' @typed data: data.frame
#'  with columns including
#'  - `time_to_event`: Time to the first AE, death or soft competing event.
#'  - `type_of_event`: 0 for censored, 1 for AE, 2 for death, 3 for soft competing event.
#'
#' @typed tau: number
#'  milestone at which Aalen-Johansen is computed.
#'
#' @typed ce: number
#'  code for competing event.
#'
#' @typedreturn vector
#'  with the following entries:
#'
#' - `ae_prob`: Estimated probability of AE.
#' - `ae_prob_var`: Variance of that estimate.
#' - `ce_prob`: Estimated probability of competing events.
#' - `ce_prob_var`: Variance of competing events.
#'
#' @export
#'
#' @references
#' \insertRef{stegherr_estimating_2021}{savvyr}
#'
#' @examples
#' set.seed(123)
#' dat <- generate_data(n = 5, cens = c(2, 5), haz_ae = 2, haz_death = 3, haz_soft = 5)
#' aalen_johansen(dat, ce = 2, tau = 4)
aalen_johansen <- function(data,
                           ce,
                           tau) {
  assert_ae_data(data)
  assert_subset(ce, c(2, 3))
  assert_true(tau > 0)
  assert_number(tau, finite = TRUE)

  data$type_of_event_accounted <- ifelse(
    ce == 2 & data$type_of_event == 3,
    0,
    ifelse(
      ce == 3 & data$type_of_event == 3,
      2,
      data$type_of_event
    )
  )

  time <- data$time_to_event
  type2 <- data$type_of_event_accounted

  # conditions
  c1 <- sum(data$type_of_event_accounted == 1)
  c2 <- sum(data$type_of_event_accounted == 2)

  if (c1 == 0) {
    ae_prob <- 0
    ae_prob_var <- 0
  }

  if (c2 == 0) {
    ce_prob <- 0
    ce_prob_var <- 0
  }

  # define auxiliary objects
  help <- data.frame(id = data$id)
  help$from <- 0
  help$time <- ifelse(time == 0, 0.001, time)
  tra <- matrix(FALSE, 2, 2)
  tra[1, 2] <- TRUE
  state_names <- as.character(0:1)


  if (c1 == 0 && c2 != 0) {
    help$to <- ifelse(type2 != 2, "cens", type2 - 1)
    etmmm <- etm::etm(help, state_names, tra, "cens", s = 0)
    setmm <- summary(etmmm)[[2]]
    trans_mat_tau <- setmm[sum(setmm$time <= tau), ]
    ce_prob <- trans_mat_tau$P
    ce_prob_var <- trans_mat_tau$var
  }


  if (c1 != 0 && c2 == 0) {
    help$to <- ifelse(type2 != 1, "cens", type2)
    etmmm <- etm::etm(help, state_names, tra, "cens", s = 0)
    setmm <- summary(etmmm)[[2]]
    trans_mat_tau <- setmm[sum(setmm$time <= tau), ]
    ae_prob <- trans_mat_tau$P
    ae_prob_var <- trans_mat_tau$var
  }

  if (c1 != 0 && c2 != 0) {
    help$to <- ifelse(!(type2 %in% c(1, 2)), "cens", type2)

    tra <- matrix(FALSE, 3, 3)
    tra[1, 2:3] <- TRUE
    state_names <- as.character(0:2)
    etmmm <- etm::etm(help, state_names, tra, "cens", s = 0)
    setmm <- summary(etmmm)

    trans_mat_tau_ae <- setmm[[2]][sum(setmm[[2]]$time <= tau), ]
    ae_prob <- trans_mat_tau_ae$P
    ae_prob_var <- trans_mat_tau_ae$var

    trans_mat_tau_ce <- setmm[[3]][sum(setmm[[3]]$time <= tau), ]
    ce_prob <- trans_mat_tau_ce$P
    ce_prob_var <- trans_mat_tau_ce$var
  }


  c(
    "ae_prob" = ae_prob,
    "ae_prob_var" = ae_prob_var,
    "ce_prob" = ce_prob,
    "ce_prob_var" = ce_prob_var
  )
}

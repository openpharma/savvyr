#' One Minus Kaplan-Meier
#'
#' This funciton calculates the one minus Kaplan-Meier estimator of adverse events (while censoring all competing events)
#' observed in `[0, tau]`.
#' Please also refer to formula (4) in
#'
#' Stegherr R, Beyersmann J, Jehl V, Rufibach K, Leverkus F, Schmoor C, Friede T.
#' Survival analysis for adverse events with varying follow-up times (SAVVY):
#' Rationale and statistical concept of a meta-analytic study. Biom J. 2021; 63:650–70.
#' https://doi.org/10.1002/bimj.201900347.
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
#' @examples
#' set.seed(123)
#' dat <- generate_data(n = 5, cens = c(2, 5), haz_ae = 2, haz_death = 3, haz_soft = 5)
#' one_minus_kaplan_meier(dat, tau = 4)
one_minus_kaplan_meier <- function(data,
                                   tau) {
  assert_data_frame(data, any.missing = FALSE, min.rows = 1, min.cols = 2)
  assert_numeric(data$time_to_event, lower = 0, finite = TRUE)
  assert_integerish(data$type_of_event, any.missing = FALSE)
  assert_subset(data$type_of_event, c(0, 1, 2, 3))
  assert_number(tau, finite = TRUE)
  assert_true(tau > 0)

  if(nrow(data %>% filter(type_of_event == 1)) == 0){
    ae_prob <- 0
    ae_prob_var <- 0
  } else {
    help <- data.frame(id = data$id)
    help$from <- 0
    help$to <- ifelse(data$type_of_event != 1, "cens", data$type_of_event)
    help$time <-ifelse(data$time_to_event == 0, 0.001, data$time_to_event)

    tra <- matrix(FALSE, 2, 2)
    tra[1, 2] <- TRUE
    state.names <-as.character(0:1)
    etmmm <-etm(help, state.names, tra, "cens", s = 0)

    ae_prob <- summary(etmmm)[[2]][sum(summary(etmmm)[[2]]$time <= tau),]$P
    ae_prob_var <- summary(etmmm)[[2]][sum(summary(etmmm)[[2]]$time <= tau),]$var
  }

  c("ae_prob" = ae_prob, "ae_prob_var" = ae_prob_var)
}

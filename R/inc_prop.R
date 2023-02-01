#' Computing the Incidence Proportion
#'
#' @param data (`data.frame`) with columns with
#'  - `time_to_event`: Time to the first AE, death or soft competing event.
#'  - `type_of_event`: 0 for censored, 1 for AE, 2 for death, 3 for soft competing event.
#'
#' @param tau (`numeric`) \cr Milestone at which incidence proportion is computed
#'
#' @return A `vector` with the following entries:
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
#'
inc_prop <- function(data, tau){

  assertDataFrame(data, types = rep("numeric", 5), any.missing = FALSE, all.missing = FALSE, min.rows = 1, ncols = 4)
  assert_number(tau, finite = TRUE)
  assert_true(tau > 0)

  n <- nrow(data)
  ae <- nrow(data["type_of_event" == 1 & "time_to_event" <= tau]) / n
  ae_prob_var <- ae * (1 - ae) / n

  # result
  c("ae_prob" = ae, "ae_prob_var" = ae_prob_var)
}

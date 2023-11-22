#' Generate Example Data
#'
#' This generates the dataset denoted by `S1` in Table 4 of
#' \insertCite{stegherr_estimating_2021;textual}{savvyr},
#' i.e. we assume constant hazards for the adverse event (AE) hazard, the
#' hazard for the competing event of death, and the hazard for the "soft"
#' competing events. Censoring is uniform in the given range.
#'
#' @typed n: count
#'   number of patients.
#' @typed cens: numeric
#'   minimum and maximum censoring time.
#' @typed haz_ae: number
#'   constant hazard for AE.
#' @typed haz_death: number
#'   constant hazard for death.
#' @typed haz_soft: number
#'   constant hazard for soft competing event.
#'
#' @typedreturn data.frame
#'   a `data.frame` with the following columns:
#'   - `id`: Patient ID.
#'   - `time_to_event`: Time to the first AE, death or soft competing event.
#'   - `type_of_event`: 0 for censored, 1 for AE, 2 for death, 3 for soft
#'     competing event.
#'   - `cens`: Censoring time.
#'
#' @export
#'
#' @references
#' \insertRef{stegherr_estimating_2021}{savvyr}
#'
#' @examples
#' set.seed(123)
#' generate_data(n = 5, cens = c(2, 5), haz_ae = 2, haz_death = 3, haz_soft = 5)
generate_data <- function(n,
                          cens,
                          haz_ae,
                          haz_death,
                          haz_soft) {
  assert_count(n, positive = TRUE)
  assert_numeric(cens, lower = 0, finite = TRUE, any.missing = FALSE, len = 2L, unique = TRUE, sorted = TRUE)
  assert_number(haz_ae, finite = TRUE)
  assert_number(haz_death, finite = TRUE)
  assert_number(haz_soft, finite = TRUE)
  haz <- c(haz_ae, haz_death, haz_soft)
  assert_true(all(haz > 0))

  result <- data.frame(
    time_to_event = rep(0, n),
    type_of_event = rep(0, n)
  )
  haz_all <- sum(haz)
  result$time_to_event <- stats::rexp(n = n, rate = haz_all)
  result$type_of_event <- 1L + stats::rbinom(
    n = n,
    size = 2,
    prob = haz / haz_all
  )
  result$cens <- stats::runif(n = n, min = cens[1L], max = cens[2L])
  result$type_of_event <- ifelse(
    result$time_to_event <= result$cens,
    result$type_of_event,
    rep(0L, n)
  )
  result$time_to_event <- pmin(result$time_to_event, result$cens)
  result$id <- seq_len(n)

  # Reorder columns.
  result[, c("id", "time_to_event", "type_of_event", "cens")]
}

# function to generate dataset with constant hazards for AE, death, and soft competing events


#' Title
#'
#' @param N
#' @param seed
#' @param min_cens
#' @param max_cens
#' @param haz_ae
#' @param haz_death
#' @param haz_soft
#'
#' @return
#' @export
#'
#' @examples
data_generation_constant_cens <- function(N,
                                          min_cens,
                                          max_cens,
                                          haz_ae,
                                          haz_death,
                                          haz_soft,
                                          seed = 57 * i + 5) {

  # status, 1 for AE, 2 for death 3 for soft competing event
  set.seed(seed)
  haz.all <- haz.AE + haz.death + haz.soft

  my.data <- data.table(time_to_event = rep(0, N), type_of_event = rep(0, N))
  my.data$time_to_event<- rexp(n = N, rate = haz.all) # event time
  my.data$type_of_event <- rbinom(n = N, size = 2,
                                  prob = c(haz.AE / haz.all, haz.death / haz.all, haz.soft / haz.all)) + 1
  # status, 1 for AE, 2 for death 3 for soft competing event
  my.data$cens <- runif(n = N, min = min.cens, max = max.cens)
  my.data$type_of_event <- as.numeric(my.data$time_to_event <= my.data$cens) * my.data$type_of_event
  my.data$time_to_event <- pmin(my.data$time_to_event, my.data$cens)
  my.data$id <- 1:N

  # reorder columns
  my.data <- my.data[, c("id", "time_to_event", "type_of_event", "cens")]
  return(my.data)
}

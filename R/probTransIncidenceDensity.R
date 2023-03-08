probTransIncidenceDensity <- function(data, tau) {
  time <- data$time_to_event
  incidence.dens <- nrow(data[type_of_event == 1 & time_to_event <= tau]) /
    sum(ifelse(time <= tau, time, tau))
  ae <- 1 - exp(-incidence.dens * tau)

  var_A_var <- nrow(data[type_of_event == 1 & time_to_event <= tau]) /
    sum(ifelse(time <= tau, time, tau))^2
  ae_var <- exp(-incidence.dens * tau)^2 * var_A_var * tau^2

  res <- c("ae_prob" = ae, "ae_prob_var" = ae_var)
  return(res)
}

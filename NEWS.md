# savvyr 0.1.0

- First CRAN version of the package.
- The package provides functions to easily conduct the improved AE analyses proposed by the SAVVY framework.

### New Features

- Estimators that do not account for competing events (incidence proportion, incidence density, Inverse Kaplan Meier).
- Estimators accounting for competing events (incidence proportion accounting for competing events and Aalen-Johansen, both first with death only as hard competing event, or using all competing events).


### bug fixes

- changed the way data is generated in function generate_data:

  result$type_of_event <- 1L + stats::rbinom(n = n, size = 2, prob = haz/haz_all)

  was replaced with

  result$type_of_event <- sample(1:3, size = n, prob = haz / haz_all, replace =      TRUE).

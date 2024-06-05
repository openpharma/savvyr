# savvyr 0.1.0

### Bug Fixes

- changed the way data is generated in function "generate_data":
  variable "type_event" is now generated using the sample function which accurately 
  reflects the specified hazard for AE, death and soft competing events.


- First CRAN version of the package.
- The package provides functions to easily conduct the improved AE analyses proposed by the SAVVY framework.

### New Features

- Estimators that do not account for competing events (incidence proportion, incidence density, Inverse Kaplan Meier).
- Estimators accounting for competing events (incidence proportion accounting for competing events and Aalen-Johansen, both first with death only as hard competing event, or using all competing events).




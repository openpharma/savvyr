list(
  linters = list(
    typed = list(
      "descriptions should start lowercase" = "^[^[:upper:]]",
      roxylint::lint_full_stop,
      NULL # disallow registration of package defaults
    ),
    param = list(
      "descriptions should start lowercase" = "^[^[:upper:]]",
      roxylint::lint_full_stop
    )
  )
)

list(
  format = function(x, name, type, default, description, ...) {
    glue::glue("(`{type}`)\\cr {description}")
  }
)

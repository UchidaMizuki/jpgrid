# str_c_replace_na <- function(..., replacement = "") {
#   args <- list2(...) %>%
#     purrr::map(purrr::partial(stringr::str_replace_na,
#                               replacement = replacement))
#   exec(stringr::str_c, !!!args)
# }

na_if_na <- function(x, y) {
  dplyr::if_else(are_na(y),
                 vec_cast(NA, x),
                 x)
}

size_match <- function(size) {
  if (inherits(size, "units")) {
    if (size >= units::set_units(1, km)) {
      size <- units::set_units(size, km)
    } else {
      size <- units::set_units(size, m)
    }
    size <- stringr::str_c(units::drop_units(size), units::deparse_unit(size))
  }
  arg_match(size, c("80km", "10km", "1km", "500m", "250m", "125m", "100m"))
  size
}

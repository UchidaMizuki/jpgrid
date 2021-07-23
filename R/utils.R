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

code_XY_to_2x2 <- function(code_X, code_Y) {
  dplyr::case_when(code_Y == 0 & code_X == 0 ~ 1L,
                   code_Y == 0 & code_X == 1 ~ 2L,
                   code_Y == 1 & code_X == 0 ~ 3L,
                   code_Y == 1 & code_X == 1 ~ 4L)
}

code_2x2_to_X <- function(code_2x2) {
  dplyr::case_when(code_2x2 %in% c(1, 3) ~ 0,
                   code_2x2 %in% c(2, 4) ~ 1)
}

code_2x2_to_Y <- function(code_2x2) {
  dplyr::case_when(code_2x2 %in% c(1, 2) ~ 0,
                   code_2x2 %in% c(3, 4) ~ 1)
}

grid_size_match <- function(size) {
  if (is_missing(size)) {
    cli_abort("{.arg size} must not be missing.")
  } else if (inherits(size, "units")) {
    size <- size |>
      units::set_units("m") |>
      units::drop_units()
  } else if (is.character(size)) {
    size <- switch(size,
                   `80km` = 80000L,
                   `10km` = 10000L,
                   `1km` = 1000L,
                   `500m` = 500L,
                   `250m` = 250L,
                   `125m` = 125L,
                   `100m` = 100L,
                   NA_integer_)
  }

  if (!size %in% c(80000L, 10000L, 1000L, 500L, 250L, 125L, 100L)) {
    sizes <- cli_vec(c("80km", "10km", "1km", "500m", "250m", "125m", "100m"),
                     style = list("vec-last" = " or "))
    cli_abort("{.arg size} must {.val {sizes}}")
  }
  size
}

grid_size <- function(grid) {
  switch(class(grid)[1L],
         grid_80km = 80000L,
         grid_10km = 10000L,
         grid_1km = 1000L,
         grid_500m = 500L,
         grid_250m = 250L,
         grid_125m = 125L,
         grid_100m = 100L)
}

code_80km_to_number <- function(code) {
  code |>
    stringr::str_extract("(?<=^<?)\\-?\\d+(?=>?$)") |>
    as.integer()
}

code_to_number <- function(code, number_min, number_max) {
  number <- as.integer(code)
  dplyr::if_else(is.na(number) | number_min <= number & number <= number_max,
                 number,
                 NA_integer_)
}

number_to_code_80km <- function(number) {
  code <- number |>
    stringr::str_pad(2L,
                     side = "left",
                     pad = "0")

  dplyr::if_else(10L <= number & number < 100L,
                 code,
                 stringr::str_c("<", number, ">"))
}

code_XY_to_2x2 <- function(code_X, code_Y) {
  dplyr::case_when(code_Y == 0L & code_X == 0L ~ 1L,
                   code_Y == 0L & code_X == 1L ~ 2L,
                   code_Y == 1L & code_X == 0L ~ 3L,
                   code_Y == 1L & code_X == 1L ~ 4L)
}

code_2x2_to_X <- function(code_2x2) {
  dplyr::case_when(code_2x2 %in% c(1L, 3L) ~ 0L,
                   code_2x2 %in% c(2L, 4L) ~ 1L)
}

code_2x2_to_Y <- function(code_2x2) {
  dplyr::case_when(code_2x2 %in% c(1L, 2L) ~ 0L,
                   code_2x2 %in% c(3L, 4L) ~ 1L)
}

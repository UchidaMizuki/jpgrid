size_match <- function(size) {
  if (inherits(size, "units")) {
    size <- size %>%
      units::set_units(m) %>%
      units::drop_units()
  } else if (is.character(size)) {
    size <- switch(size,
                   `80km` = 80000,
                   `10km` = 10000,
                   `1km` = 1000,
                   `500m` = 500,
                   `250m` = 250,
                   `125m` = 125,
                   `100m` = 100)
  }

  stopifnot(size %in% c(80000, 10000, 1000, 500, 250, 125, 100))
  size
}

code_to_number <- function(code, code_min, code_max) {
  code <- as.integer(code)
  stopifnot(is.na(code) | code_min <= code & code <= code_max)
  code
}

code_100m_to_500m <- function(code_X, code_Y) {
  dplyr::case_when(code_Y %in% 0:4 & code_X %in% 0:4 ~ 1L,
                   code_Y %in% 0:4 & code_X %in% 5:9 ~ 2L,
                   code_Y %in% 5:9 & code_X %in% 0:4 ~ 3L,
                   code_Y %in% 5:9 & code_X %in% 5:9 ~ 4L)
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

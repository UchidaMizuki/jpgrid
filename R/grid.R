# grid --------------------------------------------------------------------

new_grid <- function(size,
                     n_X = integer(),
                     n_Y = integer()) {
  new_rcrd(list_of(n_X = n_X,
                   n_Y = n_Y,
                   .ptype = integer()),
           class = switch(as.character(size),
                          "80000" = c("grid_80km", "grid"),
                          "10000" = c("grid_10km", "grid"),
                          "1000" = c("grid_1km", "grid"),
                          "500" = c("grid_500m", "grid"),
                          "250" = c("grid_250m", "grid"),
                          "125" = c("grid_125m", "grid"),
                          "100" = c("grid_100m", "grid")))
}

code_to_grid <- function(size,

                         code_X_80km = NA_integer_,
                         code_Y_80km = NA_integer_,

                         code_X_10km = NA_integer_,
                         code_Y_10km = NA_integer_,

                         code_X_1km = NA_integer_,
                         code_Y_1km = NA_integer_,

                         code_500m = NA_integer_,
                         code_250m = NA_integer_,
                         code_125m = NA_integer_,

                         code_X_100m = NA_integer_,
                         code_Y_100m = NA_integer_) {
  n_X_80km <- code_80km_to_number(code_X_80km)
  n_Y_80km <- code_80km_to_number(code_Y_80km)

  if (size == 80000L) {
    n_X <- n_X_80km
    n_Y <- n_Y_80km
  } else {
    n_X_10km <- n_X_80km * 8L + code_to_number(code_X_10km, 0L, 7L)
    n_Y_10km <- n_Y_80km * 8L + code_to_number(code_Y_10km, 0L, 7L)

    if (size == 10000L) {
      n_X <- n_X_10km
      n_Y <- n_Y_10km
    } else {
      n_X_1km <- n_X_10km * 10L + code_to_number(code_X_1km, 0L, 9L)
      n_Y_1km <- n_Y_10km * 10L + code_to_number(code_Y_1km, 0L, 9L)

      if (size == 1000L) {
        n_X <- n_X_1km
        n_Y <- n_Y_1km
      } else if (size %in% c(500L, 250L, 125L)) {
        # 500m, 250m, 125m
        code_500m <- code_to_number(code_500m, 1L, 4L)
        n_X_500m <- n_X_1km * 2L + code_2x2_to_X(code_500m)
        n_Y_500m <- n_Y_1km * 2L + code_2x2_to_Y(code_500m)

        if (size == 500L) {
          n_X <- n_X_500m
          n_Y <- n_Y_500m
        } else {
          code_250m <- code_to_number(code_250m, 1L, 4L)
          n_X_250m <- n_X_500m * 2L + code_2x2_to_X(code_250m)
          n_Y_250m <- n_Y_500m * 2L + code_2x2_to_Y(code_250m)

          if (size == 250L) {
            n_X <- n_X_250m
            n_Y <- n_Y_250m
          } else if (size == 125L) {
            code_125m <- code_to_number(code_125m, 1L, 4L)
            n_X <- n_X_250m * 2L + code_2x2_to_X(code_125m)
            n_Y <- n_Y_250m * 2L + code_2x2_to_Y(code_125m)
          }
        }
      } else if (size == 100L) {
        n_X <- n_X_1km * 10L + code_to_number(code_X_100m, 0L, 9L)
        n_Y <- n_Y_1km * 10L + code_to_number(code_Y_100m, 0L, 9L)
      }
    }
  }

  not_na_n <- !is.na(n_X) & !is.na(n_Y)
  n_X <- dplyr::if_else(not_na_n,
                        n_X,
                        NA_integer_)
  n_Y <- dplyr::if_else(not_na_n,
                        n_Y,
                        NA_integer_)
  new_grid(size = size,
           n_X = n_X,
           n_Y = n_Y)
}

grid_to_code <- function(grid) {
  grid_to_code_impl(size = grid_size(grid),
                    n_X = field(grid, "n_X"),
                    n_Y = field(grid, "n_Y"))
}

grid_to_code_impl <- function(size, n_X, n_Y) {
  if (size == 80000L) {
    list(code_X_80km = number_to_code_80km(n_X),
         code_Y_80km = number_to_code_80km(n_Y))
  } else if (size == 10000L) {
    grid_to_code_impl(n_X = n_X %/% 8L,
                      n_Y = n_Y %/% 8L,
                      size = 80000L) |>
      c(list(code_X_10km = n_X %% 8L,
             code_Y_10km = n_Y %% 8L))
  } else if (size == 1000L) {
    grid_to_code_impl(n_X = n_X %/% 10L,
                      n_Y = n_Y %/% 10L,
                      size = 10000L) |>
      c(list(code_X_1km = n_X %% 10L,
             code_Y_1km = n_Y %% 10L))
  } else if (size == 500L) {
    grid_to_code_impl(n_X = n_X %/% 2L,
                      n_Y = n_Y %/% 2L,
                      size = 1000L) |>
      c(list(code_500m = code_XY_to_2x2(code_X = n_X %% 2L,
                                        code_Y = n_Y %% 2L)))
  } else if (size == 250L) {
    grid_to_code_impl(n_X = n_X %/% 2L,
                      n_Y = n_Y %/% 2L,
                      size = 500L) |>
      c(list(code_250m = code_XY_to_2x2(code_X = n_X %% 2L,
                                        code_Y = n_Y %% 2L)))
  } else if (size == 125L) {
    grid_to_code_impl(n_X = n_X %/% 2L,
                      n_Y = n_Y %/% 2L,
                      size = 250L) |>
      c(list(code_125m = code_XY_to_2x2(code_X = n_X %% 2L,
                                        code_Y = n_Y %% 2L)))
  } else if (size == 100L) {
    grid_to_code_impl(n_X = n_X %/% 10L,
                      n_Y = n_Y %/% 10L,
                      size = 1000L) |>
      c(list(code_X_100m = n_X %% 10L,
             code_Y_100m = n_Y %% 10L))
  }
}

#' Test if the object is a grid
#'
#' @param x An object.
#' @param size A grid size.
#'
#' @return `TRUE` if the object inherits from the `grid` class.
#'
#' @export
is_grid <- function(x,
                    size = NULL) {
  if (is.null(size)) {
    inherits(x, "grid")
  } else {
    grid_size(x) == grid_size_match(size)
  }
}

# printing ----------------------------------------------------------------
#' @export
format.grid_80km <- function(x, ...) {
  code <- grid_to_code(x)
  stringr::str_c(code$code_Y_80km,
                 code$code_X_80km)
}

#' @export
format.grid_10km <- function(x, ...) {
  code <- grid_to_code(x)
  stringr::str_c(code$code_Y_80km,
                 code$code_X_80km,

                 code$code_Y_10km,
                 code$code_X_10km)
}

#' @export
format.grid_1km <- function(x, ...) {
  code <- grid_to_code(x)
  stringr::str_c(code$code_Y_80km,
                 code$code_X_80km,

                 code$code_Y_10km,
                 code$code_X_10km,

                 code$code_Y_1km,
                 code$code_X_1km)
}

#' @export
format.grid_500m <- function(x, ...) {
  code <- grid_to_code(x)
  stringr::str_c(code$code_Y_80km,
                 code$code_X_80km,

                 code$code_Y_10km,
                 code$code_X_10km,

                 code$code_Y_1km,
                 code$code_X_1km,

                 code$code_500m)
}

#' @export
format.grid_250m <- function(x, ...) {
  code <- grid_to_code(x)
  stringr::str_c(code$code_Y_80km,
                 code$code_X_80km,

                 code$code_Y_10km,
                 code$code_X_10km,

                 code$code_Y_1km,
                 code$code_X_1km,

                 code$code_500m,
                 code$code_250m)
}

#' @export
format.grid_125m <- function(x, ...) {
  code <- grid_to_code(x)
  stringr::str_c(code$code_Y_80km,
                 code$code_X_80km,

                 code$code_Y_10km,
                 code$code_X_10km,

                 code$code_Y_1km,
                 code$code_X_1km,

                 code$code_500m,
                 code$code_250m,
                 code$code_125m)
}

#' @export
format.grid_100m <- function(x, ...) {
  code <- grid_to_code(x)
  stringr::str_c(code$code_Y_80km,
                 code$code_X_80km,

                 code$code_Y_10km,
                 code$code_X_10km,

                 code$code_Y_1km,
                 code$code_X_1km,

                 code$code_Y_100m,
                 code$code_X_100m)
}

#' @export
as.character.grid <- function(x, ...) {
  format(x, ...)
}

#' @export
vec_ptype_abbr.grid_80km <- function(x) {
  "grd80k"
}

#' @export
vec_ptype_abbr.grid_10km <- function(x) {
  "grd10k"
}

#' @export
vec_ptype_abbr.grid_1km <- function(x) {
  "grd1k"
}

#' @export
vec_ptype_abbr.grid_500m <- function(x) {
  "grd500"
}

#' @export
vec_ptype_abbr.grid_250m <- function(x) {
  "grd250"
}

#' @export
vec_ptype_abbr.grid_125m <- function(x) {
  "grd125"
}

#' @export
vec_ptype_abbr.grid_100m <- function(x) {
  "grd100"
}

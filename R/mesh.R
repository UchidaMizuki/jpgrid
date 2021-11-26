# mesh --------------------------------------------------------------------
#' Regional mesh vector
#'
#' A series of functions return \code{mesh} class for each mesh size.
#' \code{mesh_auto} returns automatically determine mesh size by the largest mesh size.
#'
#' @name mesh_class
#'
#' @param x A list or vector.
#' @param strict A logical scalar. Should the number of digits in the mesh code match a given number of digits?
#'
#' @return A \code{mesh} class vector.
#'
#' @examples
#' mesh_80km("53394526313")
#' mesh_80km("53394526313", strict = FALSE)
#'
#' mesh_auto(c("53394526313", "5339358633", "533945764"))
#' mesh_auto(c("53394526313", "5339358633", "533945764"), strict = FALSE)
NULL

new_mesh <- function(size,
                     n_X = integer(),
                     n_Y = integer()) {
  new_rcrd(list_of(n_X = n_X,
                   n_Y = n_Y,
                   .ptype = integer()),
           class = switch(as.character(size),
                          "80000" = c("mesh_80km", "mesh"),
                          "10000" = c("mesh_10km", "mesh"),
                          "1000" = c("mesh_1km", "mesh"),
                          "500" = c("mesh_500m", "mesh"),
                          "250" = c("mesh_250m", "mesh"),
                          "125" = c("mesh_125m", "mesh"),
                          "100" = c("mesh_100m", "mesh")))
}

code_to_mesh <- function(size,

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
  new_mesh(size = size,
           n_X = n_X,
           n_Y = n_Y)
}

mesh_to_code <- function(mesh) {
  mesh_to_code_impl(size = mesh_size(mesh),
                    n_X = field(mesh, "n_X"),
                    n_Y = field(mesh, "n_Y"))
}

mesh_to_code_impl <- function(size, n_X, n_Y) {
  if (size == 80000L) {
    list(code_X_80km = number_to_code_80km(n_X),
         code_Y_80km = number_to_code_80km(n_Y))
  } else if (size == 10000L) {
    mesh_to_code_impl(n_X = n_X %/% 8L,
                      n_Y = n_Y %/% 8L,
                      size = 80000L) %>%
      c(list(code_X_10km = n_X %% 8L,
             code_Y_10km = n_Y %% 8L))
  } else if (size == 1000L) {
    mesh_to_code_impl(n_X = n_X %/% 10L,
                      n_Y = n_Y %/% 10L,
                      size = 10000L) %>%
      c(list(code_X_1km = n_X %% 10L,
             code_Y_1km = n_Y %% 10L))
  } else if (size == 500L) {
    mesh_to_code_impl(n_X = n_X %/% 2L,
                      n_Y = n_Y %/% 2L,
                      size = 1000L) %>%
      c(list(code_500m = code_XY_to_2x2(code_X = n_X %% 2L,
                                        code_Y = n_Y %% 2L)))
  } else if (size == 250L) {
    mesh_to_code_impl(n_X = n_X %/% 2L,
                      n_Y = n_Y %/% 2L,
                      size = 500L) %>%
      c(list(code_250m = code_XY_to_2x2(code_X = n_X %% 2L,
                                        code_Y = n_Y %% 2L)))
  } else if (size == 125L) {
    mesh_to_code_impl(n_X = n_X %/% 2L,
                      n_Y = n_Y %/% 2L,
                      size = 250L) %>%
      c(list(code_125m = code_XY_to_2x2(code_X = n_X %% 2L,
                                        code_Y = n_Y %% 2L)))
  } else if (size == 100L) {
    mesh_to_code_impl(n_X = n_X %/% 10L,
                      n_Y = n_Y %/% 10L,
                      size = 1000L) %>%
      c(list(code_X_100m = n_X %% 10L,
             code_Y_100m = n_Y %% 10L))
  }
}

#' @export
#' @rdname mesh_class
mesh_80km <- function(x,
                      strict = TRUE) {
  mesh_impl(x,
            strict = strict,
            size = 80000L)
}

#' @export
#' @rdname mesh_class
mesh_10km <- function(x,
                      strict = TRUE) {
  mesh_impl(x,
            strict = strict,
            size = 10000L)
}

#' @export
#' @rdname mesh_class
mesh_1km <- function(x,
                     strict = TRUE) {
  mesh_impl(x,
            strict = strict,
            size = 1000L)
}

#' @export
#' @rdname mesh_class
mesh_500m <- function(x,
                      strict = TRUE) {
  mesh_impl(x,
            strict = strict,
            size = 500L)
}

#' @export
#' @rdname mesh_class
mesh_250m <- function(x,
                      strict = TRUE) {
  mesh_impl(x,
            strict = strict,
            size = 250L)
}

#' @export
#' @rdname mesh_class
mesh_125m <- function(x,
                      strict = TRUE) {
  mesh_impl(x,
            strict = strict,
            size = 125L)
}

#' @export
#' @rdname mesh_class
mesh_100m <- function(x,
                      strict = TRUE) {
  mesh_impl(x,
            strict = strict,
            size = 100L)
}

#' @export
#' @rdname mesh_class
mesh_auto <- function(x,
                      strict = TRUE) {
  if (is_mesh(x)) {
    size <- mesh_size(x)
  } else {
    pattern <- stringr::str_c("^",
                              stringr::str_dup("(<\\-?\\d+>|\\d{2})", 2),
                              "(\\d*)")
    code_others <- x %>%
      stringr::str_match(pattern) %>%
      tibble::as_tibble(.name_repair = ~ c("code", "code_Y_80km", "code_X_80km", "code_others")) %>%
      tidyr::drop_na(code_others) %>%
      purrr::chuck("code_others")

    digit <- min(stringr::str_length(code_others))

    if (digit %in% 0L:1L) {
      size <- 80000L
    } else if (digit %in% 2L:3L) {
      size <- 10000L
    } else if (digit == 4L) {
      size <- 1000L
    } else if (digit == 5L) {
      size <- 500L
    } else if (digit == 6L) {
      is_size_250m <- code_others %>%
        stringr::str_extract("\\d{6}") %>%
        stringr::str_ends("[1-4]{2}")
      if (all(is_size_250m)) {
        size <- 250L
      } else {
        size <- 100L
      }
    } else if (digit >= 7L) {
      size <- 125L
    }

    size_name <- switch(as.character(size),
                        "80000" = "80km",
                        "10000" = "10km",
                        "1000" = "1km",
                        "500" = "500m",
                        "250" = "250m",
                        "125" = "125m",
                        "100" = "100m")
    message(stringr::str_glue("Guessing mesh size as `{size_name}`"))
  }

  mesh_impl(x,
            strict = strict,
            size = size)
}

mesh_impl <- function(x, strict, size) {
  if (is_mesh(x)) {
    size <- size_match(size)
    ratio <- size / mesh_size(x)

    stopifnot(ratio %% 1 == 0)

    new_mesh(size = size,
             n_X = field(x, "n_X") %/% ratio,
             n_Y = field(x, "n_Y") %/% ratio)
  } else {
    pattern_80km <- stringr::str_c("^",
                                   stringr::str_dup("(<\\-*\\d+>|\\d{2})", 2L))

    code_80km <- c("code_Y_80km", "code_X_80km")
    code_10km <- c("code_Y_10km", "code_X_10km")
    code_1km <- c("code_Y_1km", "code_X_1km")
    code_100m <- c("code_Y_100m", "code_X_100m")

    if (size == 80000L) {
      digit <- 0L
      name <- code_80km
    } else if (size == 10000L) {
      digit <- 2L
      name <- c(code_80km, code_10km)
    } else if (size == 1000L) {
      digit <- 4L
      name <- c(code_80km, code_10km, code_1km)
    } else if (size == 500L) {
      digit <- 5L
      name <- c(code_80km, code_10km, code_1km, "code_500m")
    } else if (size == 250L) {
      digit <- 6L
      name <- c(code_80km, code_10km, code_1km, "code_500m", "code_250m")
    } else if (size == 125L) {
      digit <- 7L
      name <- c(code_80km, code_10km, code_1km, "code_500m", "code_250m", "code_125m")
    } else if (size == 100L) {
      digit <- 6L
      name <- c(code_80km, code_10km, code_1km, code_100m)
    }

    if (strict) {
      strict <- "$"
    } else {
      strict <- ""
    }

    pattern <- stringr::str_c(pattern_80km,
                              stringr::str_dup("(\\d)", digit),
                              strict)
    args <- x %>%
      stringr::str_match(pattern) %>%
      tibble::as_tibble(.name_repair = ~ c("code", name))
    args <- args[-1]

    exec(code_to_mesh,
         size = size,
         !!!args)
  }
}

#' @export
#' @rdname mesh_class
is_mesh <- function(x) {
  inherits(x, "mesh")
}

# printing ----------------------------------------------------------------
#' @export
format.mesh_80km <- function(x, ...) {
  code <- mesh_to_code(x)
  stringr::str_c(code$code_Y_80km,
                 code$code_X_80km)
}

#' @export
format.mesh_10km <- function(x, ...) {
  code <- mesh_to_code(x)
  stringr::str_c(code$code_Y_80km,
                 code$code_X_80km,

                 code$code_Y_10km,
                 code$code_X_10km)
}

#' @export
format.mesh_1km <- function(x, ...) {
  code <- mesh_to_code(x)
  stringr::str_c(code$code_Y_80km,
                 code$code_X_80km,

                 code$code_Y_10km,
                 code$code_X_10km,

                 code$code_Y_1km,
                 code$code_X_1km)
}

#' @export
format.mesh_500m <- function(x, ...) {
  code <- mesh_to_code(x)
  stringr::str_c(code$code_Y_80km,
                 code$code_X_80km,

                 code$code_Y_10km,
                 code$code_X_10km,

                 code$code_Y_1km,
                 code$code_X_1km,

                 code$code_500m)
}

#' @export
format.mesh_250m <- function(x, ...) {
  code <- mesh_to_code(x)
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
format.mesh_125m <- function(x, ...) {
  code <- mesh_to_code(x)
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
format.mesh_100m <- function(x, ...) {
  code <- mesh_to_code(x)
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
as.character.mesh <- function(x, ...) format(x)

#' @export
vec_ptype_abbr.mesh_80km <- function(x) "msh80k"
#' @export
vec_ptype_abbr.mesh_10km <- function(x) "msh10k"
#' @export
vec_ptype_abbr.mesh_1km <- function(x) "msh1k"
#' @export
vec_ptype_abbr.mesh_500m <- function(x) "msh500"
#' @export
vec_ptype_abbr.mesh_250m <- function(x) "msh250"
#' @export
vec_ptype_abbr.mesh_125m <- function(x) "msh125"
#' @export
vec_ptype_abbr.mesh_100m <- function(x) "msh100"

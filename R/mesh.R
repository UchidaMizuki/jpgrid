# mesh --------------------------------------------------------------------
#' Meshcode vector
#'
#' @name mesh
#'
#' @param x meshcode
#'
#' @examples
#' mesh_80km("53394526313")
#' mesh_1km(c("53394526313", "5339358633", "533945764"))
NULL

new_mesh <- function(size,
                     n_X = integer(),
                     n_Y = integer()) {
  new_rcrd(list(n_X = n_X,
                n_Y = n_Y),
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

  if (size == 80000) {
    n_X <- n_X_80km
    n_Y <- n_Y_80km
  } else {
    n_X_10km <- n_X_80km * 8 + code_to_number(code_X_10km, 0, 7)
    n_Y_10km <- n_Y_80km * 8 + code_to_number(code_Y_10km, 0, 7)

    if (size == 10000) {
      n_X <- n_X_10km
      n_Y <- n_Y_10km
    } else {
      n_X_1km <- n_X_10km * 10 + code_to_number(code_X_1km, 0, 9)
      n_Y_1km <- n_Y_10km * 10 + code_to_number(code_Y_1km, 0, 9)

      if (size == 1000) {
        n_X <- n_X_1km
        n_Y <- n_Y_1km
      } else if (size %in% c(500, 250, 125)) {
        # 500m, 250m, 125m
        code_500m <- code_to_number(code_500m, 1, 4)
        n_X_500m <- n_X_1km * 2 + code_2x2_to_X(code_500m)
        n_Y_500m <- n_Y_1km * 2 + code_2x2_to_Y(code_500m)

        if (size == 500) {
          n_X <- n_X_500m
          n_Y <- n_Y_500m
        } else {
          code_250m <- code_to_number(code_250m, 1, 4)
          n_X_250m <- n_X_500m * 2 + code_2x2_to_X(code_250m)
          n_Y_250m <- n_Y_500m * 2 + code_2x2_to_Y(code_250m)

          if (size == 250) {
            n_X <- n_X_250m
            n_Y <- n_Y_250m
          } else if (size == 125) {
            code_125m <- code_to_number(code_125m, 1, 4)
            n_X <- n_X_250m * 2 + code_2x2_to_X(code_125m)
            n_Y <- n_Y_250m * 2 + code_2x2_to_Y(code_125m)
          }
        }
      } else if (size == 100) {
        n_X <- n_X_1km * 10 + code_to_number(code_X_100m, 0, 9)
        n_Y <- n_Y_1km * 10 + code_to_number(code_Y_100m, 0, 9)
      }
    }
  }

  not_n_na <- !is.na(n_X) & !is.na(n_Y)
  n_X <- dplyr::if_else(not_n_na,
                        n_X,
                        NA_integer_)
  n_Y <- dplyr::if_else(not_n_na,
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
  if (size == 80000) {
    list(code_X_80km = number_to_code_80km(n_X),
         code_Y_80km = number_to_code_80km(n_Y))
  } else if (size == 10000) {
    mesh_to_code_impl(n_X = n_X %/% 8,
                      n_Y = n_Y %/% 8,
                      size = 80000) %>%
      c(list(code_X_10km = n_X %% 8,
             code_Y_10km = n_Y %% 8))
  } else if (size == 1000) {
    mesh_to_code_impl(n_X = n_X %/% 10,
                      n_Y = n_Y %/% 10,
                      size = 10000) %>%
      c(list(code_X_1km = n_X %% 10,
             code_Y_1km = n_Y %% 10))
  } else if (size == 500) {
    mesh_to_code_impl(n_X = n_X %/% 2,
                      n_Y = n_Y %/% 2,
                      size = 1000) %>%
      c(list(code_500m = code_XY_to_2x2(code_X = n_X %% 2,
                                        code_Y = n_Y %% 2)))
  } else if (size == 250) {
    mesh_to_code_impl(n_X = n_X %/% 2,
                      n_Y = n_Y %/% 2,
                      size = 500) %>%
      c(list(code_250m = code_XY_to_2x2(code_X = n_X %% 2,
                                        code_Y = n_Y %% 2)))
  } else if (size == 125) {
    mesh_to_code_impl(n_X = n_X %/% 2,
                      n_Y = n_Y %/% 2,
                      size = 250) %>%
      c(list(code_125m = code_XY_to_2x2(code_X = n_X %% 2,
                                        code_Y = n_Y %% 2)))
  } else if (size == 100) {
    mesh_to_code_impl(n_X = n_X %/% 10,
                      n_Y = n_Y %/% 10,
                      size = 1000) %>%
      c(list(code_X_100m = n_X %% 10,
             code_Y_100m = n_Y %% 10))
  }
}

#' @export
#' @rdname mesh
mesh_80km <- function(x,
                      strict = T) {
  mesh_impl(x,
            strict = strict,
            size = 80000)
}

#' @export
#' @rdname mesh
mesh_10km <- function(x,
                      strict = T) {
  mesh_impl(x,
            strict = strict,
            size = 10000)
}

#' @export
#' @rdname mesh
mesh_1km <- function(x,
                     strict = T) {
  mesh_impl(x,
            strict = strict,
            size = 1000)
}

#' @export
#' @rdname mesh
mesh_500m <- function(x,
                      strict = T) {
  mesh_impl(x,
            strict = strict,
            size = 500)
}

#' @export
#' @rdname mesh
mesh_250m <- function(x,
                      strict = T) {
  mesh_impl(x,
            strict = strict,
            size = 250)
}

#' @export
#' @rdname mesh
mesh_125m <- function(x,
                      strict = T) {
  mesh_impl(x,
            strict = strict,
            size = 125)
}

#' @export
#' @rdname mesh
mesh_100m <- function(x,
                      strict = T) {
  mesh_impl(x,
            strict = strict,
            size = 100)
}

#' @export
#' @rdname mesh
mesh_auto <- function(x,
                      strict = T) {
  pattern <- stringr::str_c("^",
                            stringr::str_dup("(<\\-?\\d+>|\\d{2})", 2),
                            "(\\d*)")
  code_others <- x %>%
    stringr::str_match(pattern) %>%
    tibble::as_tibble(.name_repair = ~ c("code", "code_Y_80km", "code_X_80km", "code_others")) %>%
    tidyr::drop_na(code_others) %>%
    purrr::chuck("code_others")

  digit <- min(stringr::str_length(code_others))

  if (digit %in% 0:1) {
    size <- 80000
  } else if (digit %in% 2:3) {
    size <- 10000
  } else if (digit == 4) {
    size <- 1000
  } else if (digit == 5) {
    size <- 500
  } else if (digit == 6) {
    is_size_250m <- code_others %>%
      stringr::str_extract("\\d{6}") %>%
      stringr::str_ends("[1-4]{2}")
    if (all(is_size_250m)) {
      size <- 250
    } else {
      size <- 100
    }
  } else if (digit >= 7) {
    size <- 125
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

  mesh_impl(x,
            strict = strict,
            size = size)
}

mesh_impl <- function(x, strict, size) {
  pattern_80km <- stringr::str_c("^",
                                 stringr::str_dup("(<\\-*\\d+>|\\d{2})", 2))

  code_80km <- c("code_Y_80km", "code_X_80km")
  code_10km <- c("code_Y_10km", "code_X_10km")
  code_1km <- c("code_Y_1km", "code_X_1km")
  code_100m <- c("code_Y_100m", "code_X_100m")

  if (size == 80000) {
    digit <- 0
    name <- code_80km
  } else if (size == 10000) {
    digit <- 2
    name <- c(code_80km, code_10km)
  } else if (size == 1000) {
    digit <- 4
    name <- c(code_80km, code_10km, code_1km)
  } else if (size == 500) {
    digit <- 5
    name <- c(code_80km, code_10km, code_1km, "code_500m")
  } else if (size == 250) {
    digit <- 6
    name <- c(code_80km, code_10km, code_1km, "code_500m", "code_250m")
  } else if (size == 125) {
    digit <- 7
    name <- c(code_80km, code_10km, code_1km, "code_500m", "code_250m", "code_125m")
  } else if (size == 100) {
    digit <- 6
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
    tibble::as_tibble(.name_repair = ~ c("code", name)) %>%
    dplyr::select(!code)

  rlang::exec(code_to_mesh,
              size = size,
              !!!args)
}

#' @export
#' @rdname mesh
is_mesh <- function(x) {
  inherits(x, "mesh")
}

# printing ----------------------------------------------------------------
#' @export
#' @rdname mesh
format.mesh_80km <- function(x, ...) {
  code <- mesh_to_code(x)
  stringr::str_c(code$code_Y_80km,
                 code$code_X_80km)
}

#' @export
#' @rdname mesh
format.mesh_10km <- function(x, ...) {
  code <- mesh_to_code(x)
  stringr::str_c(code$code_Y_80km,
                 code$code_X_80km,

                 code$code_Y_10km,
                 code$code_X_10km)
}

#' @export
#' @rdname mesh
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
#' @rdname mesh
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
#' @rdname mesh
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
#' @rdname mesh
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
#' @rdname mesh
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

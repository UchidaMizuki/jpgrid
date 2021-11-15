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
  n_X_80km <- code_to_number(code_X_80km, 0, 99)
  n_Y_80km <- code_to_number(code_Y_80km, 0, 99)

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

  new_mesh(size = size,
           n_X = n_X,
           n_Y = n_Y)
}

mesh_size <- function(mesh) {
  switch(class(mesh)[1],
         mesh_80km = 80000,
         mesh_10km = 10000,
         mesh_1km = 1000,
         mesh_500m = 500,
         mesh_250m = 250,
         mesh_125m = 125,
         mesh_100m = 100)
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

# as_mesh -----------------------------------------------------------------
#' Meshcode vector
#'
#' @param x meshcode
#' @param size meshcode size
#'
#' @examples
#' mesh_80km("53394526313")
#' mesh_1km(c("53394526313", "5339358633", "533945764"))
#'
#' @name mesh
NULL

#' @export
#' @rdname mesh
as_mesh <- function(x, size, ...) {
  UseMethod("as_mesh")
}

#' @export
#' @rdname mesh
as_mesh.default <- function(x, size, ...) {
  size <- size_match(size)

  if (size == 100) {
    x <- x %>%
      stringr::str_match("^(\\d{2})(\\d{2})(\\d)(\\d)(\\d)(\\d)(\\d)(\\d)$") %>%
      tibble::as_tibble(.name_repair = ~ c("code",
                                           "code_Y_80km", "code_X_80km",
                                           "code_Y_10km", "code_X_10km",
                                           "code_Y_1km", "code_X_1km",
                                           "code_Y_100m", "code_X_100m"))
    code_to_mesh(size = size,

                 code_X_80km = x$code_X_80km,
                 code_Y_80km = x$code_Y_80km,

                 code_X_10km = x$code_X_10km,
                 code_Y_10km = x$code_Y_10km,

                 code_X_1km = x$code_X_1km,
                 code_Y_1km = x$code_Y_1km,

                 code_X_100m = x$code_X_100m,
                 code_Y_100m = x$code_Y_100m)
  } else {
    x <- x %>%
      stringr::str_match("^(\\d{2})(\\d{2})(\\d)?(\\d)?(\\d)?(\\d)?(\\d)?(\\d)?(\\d)?$") %>%
      tibble::as_tibble(.name_repair = ~ c("code",
                                           "code_Y_80km", "code_X_80km",
                                           "code_Y_10km", "code_X_10km",
                                           "code_Y_1km", "code_X_1km",
                                           "code_500m", "code_250m", "code_125m"))
    code_to_mesh(size = size,

                 code_X_80km = x$code_X_80km,
                 code_Y_80km = x$code_Y_80km,

                 code_X_10km = x$code_X_10km,
                 code_Y_10km = x$code_Y_10km,

                 code_X_1km = x$code_X_1km,
                 code_Y_1km = x$code_Y_1km,

                 code_500m = x$code_500m,
                 code_250m = x$code_250m,
                 code_125m = x$code_125m)
  }
}

#' @export
#' @rdname mesh
as_mesh.mesh <- function(x,
                         size = NULL,
                         ...) {
  if (is.null(size)) {
    x
  } else {
    size <- size_match(size)
    ratio <- size / mesh_size(x)

    stopifnot(ratio %% 1 == 0)

    new_mesh(n_X = field(x, "n_X") %/% ratio,
             n_Y = field(x, "n_Y") %/% ratio,
             size = size)
  }
}

#' @export
#' @rdname mesh
mesh_80km <- function(x) {
  as_mesh(x,
          size = 80000)
}

#' @export
#' @rdname mesh
mesh_10km <- function(x) {
  as_mesh(x,
          size = 10000)
}

#' @export
#' @rdname mesh
mesh_1km <- function(x) {
  as_mesh(x,
          size = 1000)
}

#' @export
#' @rdname mesh
mesh_500m <- function(x) {
  as_mesh(x,
          size = 500)
}

#' @export
#' @rdname mesh
mesh_250m <- function(x) {
  as_mesh(x,
          size = 250)
}

#' @export
#' @rdname mesh
mesh_125m <- function(x) {
  as_mesh(x,
          size = 125)
}

#' @export
#' @rdname mesh
mesh_100m <- function(x) {
  as_mesh(x,
          size = 100)
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

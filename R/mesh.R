mesh <- function(n_X, n_Y, size) {
  list_of(n_X = n_X,
          n_Y = n_Y,
          .ptype = integer()) %>%
    # FIXME?
    new_rcrd(class = switch(as.character(size),
                            `80000` = c("mesh_80km", "mesh"),
                            `10000` = c("mesh_10km", "mesh"),
                            `1000` = c("mesh_1km", "mesh"),
                            `500` = c("mesh_500m", "mesh"),
                            `250` = c("mesh_250m", "mesh"),
                            `125` = c("mesh_125m", "mesh"),
                            `100` = c("mesh_100m", "mesh")))
}

is_mesh <- function(x) {
  inherits(x, "mesh")
}

code_to_mesh <- function(code_X_80km = integer(),
                         code_Y_80km = integer(),

                         code_X_10km = NA_integer_,
                         code_Y_10km = NA_integer_,

                         code_X_1km = NA_integer_,
                         code_Y_1km = NA_integer_,

                         code_500m = NA_integer_,
                         code_250m = NA_integer_,
                         code_125m = NA_integer_,

                         code_X_100m = NA_integer_,
                         code_Y_100m = NA_integer_,

                         size = NULL) {
  code <- vec_recycle_common(code_X_80km = code_X_80km,
                             code_Y_80km = code_Y_80km,

                             code_X_10km = code_X_10km,
                             code_Y_10km = code_Y_10km,

                             code_X_1km = code_X_1km,
                             code_Y_1km = code_Y_1km,

                             code_500m = code_500m,
                             code_250m = code_250m,
                             code_125m = code_125m,

                             code_X_100m = code_X_100m,
                             code_Y_100m = code_Y_100m)

  n_X_80km <- code_to_number(code$code_X_80km, 0, 99)
  n_Y_80km <- code_to_number(code$code_Y_80km, 0, 99)

  if (is.null(size) && (vec_is_empty(code) || any(is.na(code$code_Y_10km))) || !is.null(size) && size == 80000) {
    size <- 80000
    n_X <- n_X_80km
    n_Y <- n_Y_80km
  } else {
    n_X_10km <- n_X_80km * 8 + code_to_number(code$code_X_10km, 0, 7)
    n_Y_10km <- n_Y_80km * 8 + code_to_number(code$code_Y_10km, 0, 7)

    if (is.null(size) && any(is.na(code$code_Y_1km)) || !is.null(size) && size == 10000) {
      size <- 10000
      n_X <- n_X_10km
      n_Y <- n_Y_10km
    } else {
      n_X_1km <- n_X_10km * 10 + code_to_number(code$code_X_1km, 0, 9)
      n_Y_1km <- n_Y_10km * 10 + code_to_number(code$code_Y_1km, 0, 9)

      if (is.null(size) && any(is.na(code$code_500m)) || !is.null(size) && size == 1000) {
        size <- 1000
        n_X <- n_X_1km
        n_Y <- n_Y_1km
      } else if (is.null(size) || !is.null(size) && size %in% c(500, 250, 125)) {
        # 500m, 250m, 125m
        code_500m <- code_to_number(code$code_500m, 1, 4)
        n_X_500m <- n_X_1km * 2 + code_2x2_to_X(code_500m)
        n_Y_500m <- n_Y_1km * 2 + code_2x2_to_Y(code_500m)

        if (is.null(size) && any(is.na(code$code_250m)) || !is.null(size) && size == 500) {
          size <- 500
          n_X <- n_X_500m
          n_Y <- n_Y_500m
        } else {
          code_250m <- code_to_number(code$code_250m, 1, 4)
          n_X_250m <- n_X_500m * 2 + code_2x2_to_X(code_250m)
          n_Y_250m <- n_Y_500m * 2 + code_2x2_to_Y(code_250m)

          if (is.null(size) && any(is.na(code$code_125m)) || !is.null(size) && size == 250) {
            size <- 250
            n_X <- n_X_250m
            n_Y <- n_Y_250m
          } else {
            size <- 125

            code_125m <- code_to_number(code$code_125m, 1, 4)
            n_X <- n_X_250m * 2 + code_2x2_to_X(code_125m)
            n_Y <- n_Y_250m * 2 + code_2x2_to_Y(code_125m)
          }
        }
      } else {
        size <- 100
        n_X <- n_X_1km * 10 + code_to_number(code$code_X_100m, 0, 9)
        n_Y <- n_Y_1km * 10 + code_to_number(code$code_Y_100m, 0, 9)
      }
    }
  }

  mesh(n_X = n_X,
       n_Y = n_Y,
       size = size)
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
  mesh_to_code_impl(n_X = field(mesh, "n_X"),
                    n_Y = field(mesh, "n_Y"),
                    size = mesh_size(mesh))
}
mesh_to_code_impl <- function(n_X, n_Y, size) {
  if (size == 80000) {
    list(code_X_80km = n_X,
         code_Y_80km = n_Y)
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
    code <- mesh_to_code_impl(n_X = n_X %/% 10,
                              n_Y = n_Y %/% 10,
                              size = 1000) %>%
      c(list(code_X_100m = n_X %% 10,
             code_Y_100m = n_Y %% 10))
    code$code_500m <- code_100m_to_500m(code_X = code$code_X_100m,
                                        code_Y = code$code_Y_100m)
    code
  }
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


# as_mesh -----------------------------------------------------------------

#' @export
as_mesh <- function(x, size, ...) {
  UseMethod("as_mesh")
}
#' @export
as_mesh.default <- function(x,
                            size = NULL,
                            ...) {
  if (!is.null(size)) {
    size <- size_match(size)
  }

  if (!is.null(size) && size == 100) {
    x <- x %>%
      stringr::str_match("^(\\d{2})(\\d{2})(\\d)(\\d)(\\d)(\\d)(\\d)(\\d)$") %>%
      tibble::as_tibble(.name_repair = ~ c("code",
                                           "code_Y_80km", "code_X_80km",
                                           "code_Y_10km", "code_X_10km",
                                           "code_Y_1km", "code_X_1km",
                                           "code_Y_100m", "code_X_100m"))
    code_to_mesh(code_X_80km = x$code_X_80km,
                 code_Y_80km = x$code_Y_80km,

                 code_X_10km = x$code_X_10km,
                 code_Y_10km = x$code_Y_10km,

                 code_X_1km = x$code_X_1km,
                 code_Y_1km = x$code_Y_1km,

                 code_X_100m = x$code_X_100m,
                 code_Y_100m = x$code_Y_100m,

                 size = size)
  } else {
    x <- x %>%
      stringr::str_match("^(\\d{2})(\\d{2})(\\d)?(\\d)?(\\d)?(\\d)?(\\d)?(\\d)?(\\d)?$") %>%
      tibble::as_tibble(.name_repair = ~ c("code",
                                           "code_Y_80km", "code_X_80km",
                                           "code_Y_10km", "code_X_10km",
                                           "code_Y_1km", "code_X_1km",
                                           "code_500m", "code_250m", "code_125m"))
    code_to_mesh(code_X_80km = x$code_X_80km,
                 code_Y_80km = x$code_Y_80km,

                 code_X_10km = x$code_X_10km,
                 code_Y_10km = x$code_Y_10km,

                 code_X_1km = x$code_X_1km,
                 code_Y_1km = x$code_Y_1km,

                 code_500m = x$code_500m,
                 code_250m = x$code_250m,
                 code_125m = x$code_125m,

                 size = size)
  }
}
#' @export
as_mesh.mesh <- function(x,
                         size = NULL,
                         ...) {
  if (is.null(size)) {
    x
  } else {
    size <- size_match(size)
    ratio <- size / mesh_size(x)

    stopifnot(ratio %% 1 == 0)

    mesh(n_X = field(x, "n_X") %/% ratio,
         n_Y = field(x, "n_Y") %/% ratio,
         size = size)
  }
}

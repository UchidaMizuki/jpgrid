#' @export
mesh <- function(code_Y_80km = integer(),
                 code_X_80km = integer(),

                 code_Y_10km = NA_integer_,
                 code_X_10km = NA_integer_,

                 code_Y_1km = NA_integer_,
                 code_X_1km = NA_integer_,

                 code_500m = NA_integer_,
                 code_250m = NA_integer_,
                 code_125m = NA_integer_,

                 code_Y_100m = NA_integer_,
                 code_X_100m = NA_integer_,

                 size = NULL,
                 is_mesh100m = F) {
  if (!is_null(size)) {
    if (inherits(size, "units")) {
      if (size >= units::set_units(1, km)) {
        size <- units::set_units(size, km)
      } else {
        size <- units::set_units(size, m)
      }
      size <- stringr::str_c(units::drop_units(size), units::deparse_unit(size))
    }
    arg_match(size, c("80km", "10km", "1km", "500m", "250m", "125m", "100m"))
  }

  stopifnot(!xor(are_na(code_Y_80km), are_na(code_X_80km)),
            !xor(are_na(code_Y_10km), are_na(code_X_10km)),
            !xor(are_na(code_Y_1km), are_na(code_X_1km)),
            !xor(are_na(code_Y_100m), are_na(code_X_100m)))

  res <- vec_recycle_common(code_Y_80km = code_Y_80km,
                            code_X_80km = code_X_80km,

                            code_Y_10km = code_Y_10km,
                            code_X_10km = code_X_10km,

                            code_Y_1km = code_Y_1km,
                            code_X_1km = code_X_1km,

                            code_500m = code_500m,
                            code_250m = code_250m,
                            code_125m = code_125m,

                            code_Y_100m = code_Y_100m,
                            code_X_100m = code_X_100m) %>%
    as_list_of(.ptype = integer())

  if (is_mesh100m) {
    res$code_500m <- dplyr::case_when(res$code_Y_100m %in% 0:4 & res$code_X_100m %in% 0:4 ~ 1L,
                                      res$code_Y_100m %in% 0:4 & res$code_X_100m %in% 5:9 ~ 2L,
                                      res$code_Y_100m %in% 5:9 & res$code_X_100m %in% 0:4 ~ 3L,
                                      res$code_Y_100m %in% 5:9 & res$code_X_100m %in% 5:9 ~ 4L)

  }

  if (is_null(size) && any(are_na(code_Y_10km)) || !is_null(size) && size == "80km") {
    stopifnot(are_na(code_Y_80km) | stringr::str_length(code_Y_80km) == 2 & stringr::str_length(code_X_80km) == 2)

    size <- "80km"
    res <- res[c("code_Y_80km", "code_X_80km")]

  } else if (is_null(size) && any(are_na(code_Y_1km)) || !is_null(size) && size == "10km") {
    stopifnot(are_na(code_Y_10km) | code_Y_10km %in% 0:7 & code_X_10km %in% 0:7)

    size <- "10km"
    res <- res[c("code_Y_80km", "code_X_80km",
                 "code_Y_10km", "code_X_10km")] %>%
      purrr::modify_at(c("code_Y_80km", "code_X_80km"),
                       purrr::partial(na_if_na,
                                      y = code_Y_10km))

  } else if (is_null(size) && any(are_na(code_500m)) || !is_null(size) && size == "1km") {
    stopifnot(are_na(code_Y_1km) | code_Y_1km %in% 0:9 & code_X_1km %in% 0:9)

    size <- "1km"
    res <- res[c("code_Y_80km", "code_X_80km",
                 "code_Y_10km", "code_X_10km",
                 "code_Y_1km", "code_X_1km")] %>%
      purrr::modify_at(c("code_Y_80km", "code_X_80km",
                         "code_Y_10km", "code_X_10km"),
                       purrr::partial(na_if_na,
                                      y = code_Y_1km))

  } else if (is_null(size) && any(are_na(code_250m)) || !is_null(size) && size == "500m") {
    stopifnot(are_na(code_500m) | code_500m %in% 1:4)

    size <- "500m"
    res <- res[c("code_Y_80km", "code_X_80km",
                 "code_Y_10km", "code_X_10km",
                 "code_Y_1km", "code_X_1km",
                 "code_500m")] %>%
      purrr::modify_at(c("code_Y_80km", "code_X_80km",
                         "code_Y_10km", "code_X_10km",
                         "code_Y_1km", "code_X_1km"),
                       purrr::partial(na_if_na,
                                      y = code_500m))

  } else if (is_null(size) && any(are_na(code_125m)) || !is_null(size) && size == "250m") {
    stopifnot(are_na(code_250m) | code_250m %in% 1:4)

    size <- "250m"
    res <- res[c("code_Y_80km", "code_X_80km",
                 "code_Y_10km", "code_X_10km",
                 "code_Y_1km", "code_X_1km",
                 "code_500m", "code_250m")] %>%
      purrr::modify_at(c("code_Y_80km", "code_X_80km",
                         "code_Y_10km", "code_X_10km",
                         "code_Y_1km", "code_X_1km",
                         "code_500m"),
                       purrr::partial(na_if_na,
                                      y = code_250m))

  } else if (is_null(size) || !is_null(size) && size == "125m") {
    stopifnot(are_na(code_125m) | code_125m %in% 1:4)

    size <- "125m"
    res <- res[c("code_Y_80km", "code_X_80km",
                 "code_Y_10km", "code_X_10km",
                 "code_Y_1km", "code_X_1km",
                 "code_500m", "code_250m", "code_125m")] %>%
      purrr::modify_at(c("code_Y_80km", "code_X_80km",
                         "code_Y_10km", "code_X_10km",
                         "code_Y_1km", "code_X_1km",
                         "code_500m", "code_250m"),
                       purrr::partial(na_if_na,
                                      y = code_125m))

  } else if (!is_null(size) && size == "100m") {
    stopifnot(are_na(code_Y_100m) | code_Y_100m %in% 0:9 & code_X_100m %in% 0:9)

    size <- "100m"
    res <- res[c("code_Y_80km", "code_X_80km",
                 "code_Y_10km", "code_X_10km",
                 "code_Y_1km", "code_X_1km",
                 "code_500m", "code_100m")] %>%
      purrr::modify_at(c("code_Y_80km", "code_X_80km",
                         "code_Y_10km", "code_X_10km",
                         "code_Y_1km", "code_X_1km"),
                       purrr::partial(na_if_na,
                                      y = code_100m))
  }

  res %>%
    as_list_of(.ptype = integer()) %>%
    new_rcrd(class = switch(size,

                            `80km` = "mesh_80km",
                            `10km` = c("mesh_10km", "mesh_80km"),
                            `1km` = c("mesh_1km", "mesh_10km", "mesh_80km"),

                            `500m` = c("mesh_500m", "mesh_1km", "mesh_10km", "mesh_80km"),
                            `250m` = c("mesh_250m", "mesh_500m", "mesh_1km", "mesh_10km", "mesh_80km"),
                            `125m` = c("mesh_125m", "mesh_250m", "mesh_500m", "mesh_1km", "mesh_10km", "mesh_80km"),

                            `100m` = c("mesh_100m", "mesh_1km", "mesh_10km", "mesh_80km")))
}

# printing ----------------------------------------------------------------

#' @export
format.mesh_80km <- function(x, ...) {
  stringr::str_c(field(x, "code_Y_80km"),
                 field(x, "code_X_80km"))
}
#' @export
format.mesh_10km <- function(x, ...) {
  stringr::str_c(NextMethod(),
                 field(x, "code_Y_10km"),
                 field(x, "code_X_10km"))
}
#' @export
format.mesh_1km <- function(x, ...) {
  stringr::str_c(NextMethod(),
                 field(x, "code_Y_1km"),
                 field(x, "code_X_1km"))
}
#' @export
format.mesh_500m <- function(x, ...) {
  stringr::str_c(NextMethod(),
                 field(x, "code_500m"))
}
#' @export
format.mesh_250m <- function(x, ...) {
  stringr::str_c(NextMethod(),
                 field(x, "code_250m"))
}
#' @export
format.mesh_125m <- function(x, ...) {
  stringr::str_c(NextMethod(),
                 field(x, "code_125m"))
}
#' @export
format.mesh_100m <- function(x, ...) {
  stringr::str_c(NextMethod(),
                 field(x, "code_Y_100m"),
                 field(x, "code_X_100m"))
}

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

#' @export
is_mesh <- function(x) inherits(x, "mesh")

#' @export
as_mesh <- function(x,
                    size = NULL,
                    is_mesh100m = F) {
  if (inherits(x, "mesh_80km") && is_null(size)) {
    x
  } else if (inherits(x, "mesh_80km")) {
    args <- fields(x) %>%
      purrr::map(~ field(x, .x))
    names(args) <- fields(x)
    exec(mesh, !!!args,
         size = size)
  } else if (is_mesh100m) {
    x %>%
      stringr::str_match("^(\\d{2})(\\d{2})(\\d)(\\d)(\\d)(\\d)(\\d)(\\d)$") %>%
      tibble::as_tibble(.name_repair = ~ c("x",
                                           "code_Y_80km", "code_X_80km",
                                           "code_Y_10km", "code_X_10km",
                                           "code_Y_1km", "code_X_1km",
                                           "code_Y_100m", "code_X_100m")) %>%
      dplyr::select(!x) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(),
                                  as.integer),
                    mesh = mesh(code_Y_80km, code_X_80km,
                                code_Y_10km, code_X_10km,
                                code_Y_1km, code_X_1km,
                                code_Y_100m = code_Y_100m,
                                code_X_100m = code_X_100m,
                                size = "100m",
                                is_mesh100m = is_mesh100m)) %>%
      dplyr::pull(mesh)
  } else {
    x %>%
      stringr::str_match("^(\\d{2})(\\d{2})(\\d)?(\\d)?(\\d)?(\\d)?(\\d)?(\\d)?(\\d)?$") %>%
      tibble::as_tibble(.name_repair = ~ c("x",
                                           "code_Y_80km", "code_X_80km",
                                           "code_Y_10km", "code_X_10km",
                                           "code_Y_1km", "code_X_1km",
                                           "code_500m", "code_250m", "code_125m")) %>%
      dplyr::select(!x) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(),
                                  as.integer),
                    mesh = mesh(code_Y_80km, code_X_80km,
                                code_Y_10km, code_X_10km,
                                code_Y_1km, code_X_1km,
                                code_500m = code_500m,
                                code_250m = code_250m,
                                code_125m = code_125m,
                                size = size,
                                is_mesh100m = is_mesh100m)) %>%
      dplyr::pull(mesh)
  }
}

# mesh1km_to_XY <- function(mesh1km,
#                           center = T) {
#   length_X <- 1 / 8 / 10
#   length_Y <- 1 / 1.5 / 8 / 10
#   res <- mesh1km %>%
#     str_match("^(\\d{2})(\\d{2})(\\d)(\\d)(\\d)(\\d)$") %>%
#     as_tibble(.name_repair = ~ c(".", "digit_1to2", "digit_3to4", str_c("digit_", 5:8))) %>%
#     select(-.) %>%
#     mutate(across(everything(),
#                   as.numeric)) %>%
#     mutate(X_center = 100 +
#              digit_3to4 +
#              digit_6 / 8 +
#              digit_8 / 8 / 10 +
#              length_X / 2,
#            Y_center = digit_1to2 / 1.5 +
#              digit_5 / 1.5 / 8 +
#              digit_7 / 1.5 / 8 / 10 +
#              length_Y / 2,
#            .keep = "unused")
#   if (!center) {
#     res <- res %>%
#       mutate(X_min = X_center - length_X / 2,
#              X_max = X_center + length_X / 2,
#              Y_min = Y_center - length_Y / 2,
#              Y_max = Y_center + length_Y / 2,
#              .keep = "unused")
#   }
#   res
# }
# XY_to_mesh1km <- function(X, Y) {
#   Y <- Y * 1.5
#   digit_1to2 <- floor(Y)
#   Y <- Y - digit_1to2
#   X <- X - 100
#   digit_3to4 <- floor(X)
#   X <- X - digit_3to4
#   Y <- Y * 8
#   digit_5 <- floor(Y)
#   Y <- Y - digit_5
#   X <- X * 8
#   digit_6 <- floor(X)
#   X <- X - digit_6
#   Y <- Y * 10
#   digit_7 <- floor(Y)
#   X <- X * 10
#   digit_8 <- floor(X)
#   str_c(digit_1to2, digit_3to4, digit_5, digit_6, digit_7, digit_8)
# }

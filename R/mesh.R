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
                 is_mesh_100m = F) {
  if (!is_null(size)) {
    size <- size_match(size)
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

  if (is_mesh_100m || !is_null(size) && size == "100m") {
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
                 "code_500m",
                 "code_Y_100m", "code_X_100m")] %>%
      purrr::modify_at(c("code_Y_80km", "code_X_80km",
                         "code_Y_10km", "code_X_10km",
                         "code_Y_1km", "code_X_1km"),
                       purrr::partial(na_if_na,
                                      y = code_Y_100m))
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
as.character.mesh_80km <- function(x, ...) format(x)

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
as_mesh <- function(x,
                    size = NULL,
                    ...) {
  UseMethod("as_mesh")
}
#' @export
as_mesh.default <- function(x,
                            size = NULL,
                            is_mesh_100m = F) {
  if (is_mesh_100m) {
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
                                is_mesh_100m = T)) %>%
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
                                is_mesh_100m = F)) %>%
      dplyr::pull(mesh)
  }
}
#' @export
as_mesh.mesh_80km <- function(x,
                              size = NULL) {
  if (is_null(size)) {
    x
  } else {
    args <- fields(x) %>%
      purrr::map(~ field(x, .x))
    names(args) <- fields(x)
    exec(mesh, !!!args,
         size = size)
  }
}

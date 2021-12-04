#' Converting sfc points to regional meshes
#'
#' @param point A \code{sfc_POINT} vector.
#' @inheritParams size
#'
#' @return A \code{mesh} class vector.
#'
#' @export
point_to_mesh <- function(point, size) {
  stopifnot(inherits(point, "sfc_POINT"))

  point <- point %>%
    sf::st_coordinates() %>%
    tibble::as_tibble()

  XY_to_mesh(X = point$X,
             Y = point$Y,
             size = size)
}

#' Converting sfc geometries to regional meshes
#'
#' @param geometry A \code{sfc} vector.
#' @inheritParams size
#' @param .predicate A \code{.predicate} parameter for \code{sf::st_filter} function.
#'
#' @return A list of \code{mesh} class vectors.
#'
#' @export
geometry_to_mesh <- function(geometry, size,
                             .predicate = sf::st_intersects) {
  if (!inherits(geometry, "sfc")) {
    geometry <- sf::st_as_sfc(geometry)
  }

  mesh <- geometry %>%
    purrr::map(sf::st_bbox) %>%
    bbox_to_mesh(size = size) %>%
    purrr::modify(function(mesh) {
      tibble::tibble(mesh = mesh) %>%
        sf::st_set_geometry(mesh_as_sfc(mesh))
    })

  purrr::map2(mesh, geometry,
              function(mesh, geometry) {
                mesh %>%
                  sf::st_filter(geometry,
                                .predicate = .predicate) %>%
                  purrr::chuck("mesh")
              })
}

#' Converting bbox to regional meshes
#'
#' @param bbox A \code{bbox} or a list of \code{bbox}.
#' @inheritParams size
#'
#' @return A \code{mesh} vector (when bbox is a \code{bbox}) or A list of \code{mesh} vectors (when bbox is a list of \code{bbox}).
#'
#' @export
bbox_to_mesh <- function(bbox, size) {
  if (is.list(bbox)) {
    bbox %>%
      purrr::map(function(bbox) {
        bbox_to_mesh(bbox, size)
      })
  } else {
    bbox <- sf::st_bbox(bbox)
    size <- size_match(size)

    mesh_min <- XY_to_mesh(X = bbox[["xmin"]],
                           Y = bbox[["ymin"]],
                           size = size)
    n_X_min <- field(mesh_min, "n_X")
    n_Y_min <- field(mesh_min, "n_Y")

    mesh_max <- XY_to_mesh(X = bbox[["xmax"]],
                           Y = bbox[["ymax"]],
                           size = size)
    n_X_max <- field(mesh_max, "n_X")
    n_Y_max <- field(mesh_max, "n_Y")

    n_XY <- tidyr::expand_grid(n_X = n_X_min:n_X_max,
                               n_Y = n_Y_min:n_Y_max)

    new_mesh(size = size,
             n_X = n_XY$n_X,
             n_Y = n_XY$n_Y)
  }
}

#' Converting regional meshes to sfc geometries
#'
#' @param x A \code{mesh} vector.
#' @param as_points Return the center points of the meshes or not?
#' @param crs Coordinate reference system.
#'
#' @return A \code{sfc_POLYGON} vector (\code{as_points = FALSE}) or a \code{sfc_POINT} vector ((\code{as_points = TRUE})).
#'
#' @export
mesh_as_sfc <- function(x,
                        as_points = FALSE,
                        crs = sf::NA_crs_) {
  stopifnot(is_mesh(x))

  geometry <- tibble::tibble(mesh = x) %>%
    vec_unique()
  geometry <- vec_slice(geometry ,
                        !is.na(geometry$mesh))

  if (!as_points) {
    XY <- mesh_to_XY(geometry$mesh,
                     center = FALSE)
    geometry$geometry <- list(XY$X_min, XY$Y_min, XY$X_max, XY$Y_max) %>%
      purrr::pmap(function(X_min, Y_min, X_max, Y_max) {
        if (is.na(X_min) || is.na(Y_min) || is.na(X_max) || is.na(Y_max)) {
          sf::st_polygon() %>%
            sf::st_sfc(crs = crs)
        } else {
          sf::st_bbox(c(xmin = X_min,
                        ymin = Y_min,
                        xmax = X_max,
                        ymax = Y_max)) %>%
            sf::st_as_sfc(crs = crs)
        }
      }) %>%
      purrr::reduce(c)
  } else {
    geometry$geometry <- mesh_to_XY(geometry$mesh,
                                    center = TRUE) %>%
      sf::st_as_sf(coords = c("X", "Y"),
                   crs = crs) %>%
      sf::st_geometry()
  }

  tibble::tibble(mesh = x) %>%
    dplyr::left_join(geometry,
                     by = "mesh") %>%
    purrr::chuck("geometry")
}

#' Converting data frame containing regional meshes to sf
#'
#' @param x A data frame.
#' @param as_points Return the center points of the meshes or not?
#' @param crs Coordinate reference system.
#' @param mesh_column_name A scalar character.
#' @param ... passed on to \code{sf::st_as_sf()}.
#'
#' @return A \code{sf} object.
#'
#' @export
mesh_as_sf <- function(x,
                       as_points = FALSE,
                       crs = sf::NA_crs_,
                       mesh_column_name = NULL,
                       ...) {
  stopifnot(is.data.frame(x))

  if (is.null(mesh_column_name)) {
    i <- x %>%
      purrr::map_lgl(is_mesh)
    mesh_column_name <- names(x) %>%
      vec_slice(i) %>%
      vec_slice(1L)
  }
  mesh <- x[[mesh_column_name]]

  x %>%
    sf::st_set_geometry(mesh %>%
                          mesh_as_sfc(as_points = as_points,
                                      crs = crs)) %>%
    sf::st_as_sf(...)
}

#' @export
plot.mesh <- function(x, y,
                      as_points = FALSE,
                      ...) {
  stopifnot(missing(y))

  x %>%
    mesh_as_sfc(as_points = as_points) %>%
    plot(...)
}

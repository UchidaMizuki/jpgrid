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
        sf::st_set_geometry(mesh_to_polygon(mesh))
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
    bbox <- bbox %>%
      purrr::map_dfr(function(bbox) {
        bbox <- sf::st_bbox(bbox)

        tibble::tibble(X_min = bbox[["xmin"]],
                       Y_min = bbox[["ymin"]],
                       X_max = bbox[["xmax"]],
                       Y_max = bbox[["ymax"]])
      })

    mesh_grid(X_min = bbox$X_min,
              Y_min = bbox$Y_min,
              X_max = bbox$X_max,
              Y_max = bbox$Y_max,
              size = size)
  } else {
    bbox <- sf::st_bbox(bbox)

    mesh_grid(X_min = bbox[["xmin"]],
              Y_min = bbox[["ymin"]],
              X_max = bbox[["xmax"]],
              Y_max = bbox[["ymax"]],
              size = size) %>%
      dplyr::first()
  }
}

#' Converting regional meshes to sfc geometries
#'
#' @param x A \code{mesh} vector.
#' @param centroid Return the mesh centroids or not?
#' @param crs Coordinate reference system.
#'
#' @return A \code{sfc_POLYGON} vector (\code{centroid = FALSE}) or a \code{sfc_POINT} vector ((\code{centroid = TRUE})).
#'
#' @export
mesh_as_sfc <- function(x,
                        centroid = FALSE,
                        crs = sf::NA_crs_) {
  stopifnot(is_mesh(x))

  geometry <- tibble::tibble(mesh = x) %>%
    vec_unique()
  geometry <- vec_slice(geometry ,
                        !is.na(geometry$mesh))

  if (!centroid) {
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
#' @param centroid Return the mesh centroids or not?
#' @param crs Coordinate reference system.
#' @param ... passed on to \code{sf::st_as_sf()}.
#'
#' @return A \code{sf} object.
#'
#' @export
mesh_as_sf <- function(x,
                       centroid = FALSE,
                       crs = sf::NA_crs_,
                       ...) {
  stopifnot(is.data.frame(x))

  i <- x %>%
    purrr::map_lgl(is_mesh)
  x %>%
    sf::st_set_geometry(x[i][[1L]] %>%
                          mesh_as_sfc(centroid = centroid,
                                      crs = crs)) %>%
    sf::st_as_sf(...)
}

#' @export
plot.mesh <- function(x, y,
                      centroid = FALSE,
                      ...) {
  stopifnot(missing(y))

  x %>%
    mesh_as_sfc(centroid = centroid) %>%
    plot(...)
}

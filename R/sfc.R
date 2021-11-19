#' Converting sfc points to meshes
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

#' Converting meshes to sfc geometries
#'
#' @name sfc
#'
#' @inheritParams mesh
#' @param crs Coordinate reference system.
#'
#' @return \code{mesh_to_polygon} returns a \code{sfc_POLYGON} vector.
#' \code{mesh_to_point} returns a \code{sfc_POINT} vector.
NULL

mesh_to_sfc <- function(mesh,
                        type = "POLYGON",
                        crs = sf::NA_crs_) {
  stopifnot(is_mesh(mesh))
  arg_match(type, c("POLYGON", "POINT"))

  geom <- tibble::tibble(mesh = mesh) %>%
    vec_unique()
  geom <- vec_slice(geom,
                    !is.na(geom$mesh))

  if (type == "POLYGON") {
    XY <- mesh_to_XY(geom$mesh,
                     center = FALSE)
    geom$geom <- list(XY$X_min, XY$Y_min, XY$X_max, XY$Y_max) %>%
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
    geom$geom <- mesh_to_XY(geom$mesh,
                            center = TRUE) %>%
      sf::st_as_sf(coords = c("X", "Y"),
                   crs = crs) %>%
      sf::st_geometry()
  }

  tibble::tibble(mesh = mesh) %>%
    dplyr::left_join(geom,
                     by = "mesh") %>%
    sf::st_as_sf() %>%
    sf::st_geometry()
}

#' @export
#'
#' @rdname sfc
mesh_to_polygon <- function(mesh,
                            crs = sf::NA_crs_) {
  mesh %>%
    mesh_to_sfc(type = "POLYGON",
                crs = crs)
}

#' @export
#'
#' @rdname sfc
mesh_to_point <- function(mesh,
                          crs = sf::NA_crs_) {
  mesh %>%
    mesh_to_sfc(type = "POINT",
                crs = crs)
}

#' @export
plot.mesh <- function(x, y,
                      type = "POLYGON",
                      ...) {
  stopifnot(missing(y))

  x %>%
    mesh_to_sfc(type = type) %>%
    plot(...)
}

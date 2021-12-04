#' Zoom-in regional meshes
#'
#' `r lifecycle::badge("deprecated")`
#' Use \code{mesh_subdivide()} instead of \code{mesh_zoomin()}.
#' \code{mesh_zoomin} makes the regional meshes finer.
#'
#' @inheritParams mesh
#' @inheritParams size
#'
#' @return A list of \code{mesh} class vector.
#'
#' @export
mesh_zoomin <- function(mesh, size) {
  lifecycle::deprecate_warn("1.0.0", "mesh_zoomin()", "mesh_subdivide()")

  mesh_subdivide(mesh, size)
}

#' Zoom-out regional meshes
#'
#' `r lifecycle::badge("deprecated")`
#' \code{mesh_zoomout} makes the regional meshes coarser.
#'
#' @inheritParams mesh
#' @inheritParams size
#'
#' @return A \code{mesh} class vector.
#'
#' @export
mesh_zoomout <- function(mesh, size) {
  lifecycle::deprecate_warn("1.0.0", "mesh_zoomout()")

  stopifnot(is_mesh(mesh))
  mesh_impl(mesh,
            strict = TRUE,
            size = size)
}

#' Converting regional meshes to sfc polygons
#'
#' `r lifecycle::badge("deprecated")`
#' Use \code{mesh_as_sfc()} instead of \code{mesh_to_polygon()}.
#'
#' @inheritParams mesh
#' @param crs Coordinate reference system.
#'
#' @return A \code{sfc_POLYGON} vector.
#'
#' @export
mesh_to_polygon <- function(mesh,
                            crs = sf::NA_crs_) {
  lifecycle::deprecate_warn("1.0.0", "mesh_to_polygon()", "mesh_as_sfc()")

  mesh %>%
    mesh_as_sfc(crs = crs)
}

#' Converting regional meshes to sfc points
#'
#' `r lifecycle::badge("deprecated")`
#' Use \code{mesh_as_sfc(as_points = TRUE)} instead of \code{mesh_to_polygon()}.
#'
#' @inheritParams mesh
#' @param crs Coordinate reference system.
#'
#' @return A \code{sfc_POINT} vector.
#'
#' @export
mesh_to_point <- function(mesh,
                          crs = sf::NA_crs_) {
  lifecycle::deprecate_warn("1.0.0", "mesh_to_polygon()", "mesh_as_sfc(as_points)")

  mesh %>%
    mesh_as_sfc(as_points = TRUE,
                crs = crs)
}

#' Create regional mesh grids
#'
#' `r lifecycle::badge("deprecated")`
#'
#' @param X_min A numeric vector.
#' @param Y_min A numeric vector.
#' @param X_max A numeric vector.
#' @param Y_max A numeric vector.
#' @inheritParams size
#'
#' @return A list of \code{mesh} vectors.
#'
#' @export
mesh_grid <- function(X_min, Y_min, X_max, Y_max, size) {
  lifecycle::deprecate_warn("1.0.0", "mesh_grid()")

  size <- size_match(size)

  mesh_min <- XY_to_mesh(X = X_min,
                         Y = Y_min,
                         size = size)
  n_X_min <- field(mesh_min, "n_X")
  n_Y_min <- field(mesh_min, "n_Y")

  mesh_max <- XY_to_mesh(X = X_max,
                         Y = Y_max,
                         size = size)
  n_X_max <- field(mesh_max, "n_X")
  n_Y_max <- field(mesh_max, "n_Y")

  purrr::pmap(list(n_X_min, n_Y_min, n_X_max, n_Y_max),
              function(n_X_min, n_Y_min, n_X_max, n_Y_max) {
                n_XY <- tidyr::expand_grid(n_X = n_X_min:n_X_max,
                                           n_Y = n_Y_min:n_Y_max)

                new_mesh(size = size,
                         n_X = n_XY$n_X,
                         n_Y = n_XY$n_Y)
              })
}

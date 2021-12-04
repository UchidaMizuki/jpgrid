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
#' Use \code{mesh_as_sfc(centroid = TRUE)} instead of \code{mesh_to_polygon()}.
#'
#' @inheritParams mesh
#' @param crs Coordinate reference system.
#'
#' @return A \code{sfc_POINT} vector.
#'
#' @export
mesh_to_point <- function(mesh,
                          crs = sf::NA_crs_) {
  lifecycle::deprecate_warn("1.0.0", "mesh_to_polygon()", "mesh_as_sfc(centroid)")

  mesh %>%
    mesh_as_sfc(centroid = TRUE,
                crs = crs)
}

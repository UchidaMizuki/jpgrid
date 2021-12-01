#' Zoom-in regional meshes
#'
#' `r lifecycle::badge("deprecated")`
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
#' @export
#'
#' @inheritParams mesh
#' @inheritParams size
#'
#' @return A \code{mesh} class vector.
mesh_zoomout <- function(mesh, size) {
  lifecycle::deprecate_warn("1.0.0", "mesh_zoomout()")

  stopifnot(is_mesh(mesh))
  mesh_impl(mesh,
            strict = TRUE,
            size = size)
}

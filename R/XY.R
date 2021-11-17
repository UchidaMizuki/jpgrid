#' Conversion between mesh and coordinates (longitude and latitude)
#'
#' @name XY

#' @export
#'
#' @rdname XY
#'
#' @param X A numeric vector of longitude.
#' @param Y A numeric vector of latitude.
#' @param size A mesh size.
#'
#' @return \code{XY_to_mesh} returns a \code{mesh} class vector.
XY_to_mesh <- function(X, Y, size) {
  size <- size_match(size)

  length_X <- size / 80000
  length_Y <- length_X / 1.5

  new_mesh(n_X = (X - 100) %/% length_X,
           n_Y = Y %/% length_Y,
           size = size)
}

#' @export
#'
#' @rdname XY
#'
#' @param mesh A \code{mesh} class vector.
#' @param center Should the center point of the mesh be returned? Otherwise the end points will be returned.
#'
#' @return \code{mesh_to_XY} returns a tbl.
mesh_to_XY <- function(mesh, center = T, ...) {
  stopifnot(is_mesh(mesh))

  length_X <- mesh_size(mesh) / 80000
  length_Y <- length_X / 1.5

  n_X <- field(mesh, "n_X")
  n_Y <- field(mesh, "n_Y")

  if (center) {
    tibble::tibble(X = 100 + length_X * (n_X + .5),
                   Y = length_Y * (n_Y + .5))
  } else {
    tibble::tibble(X_min = 100 + length_X * n_X,
                   Y_min = length_Y * n_Y,
                   X_max = X_min + length_X,
                   Y_max = Y_min + length_Y)
  }
}

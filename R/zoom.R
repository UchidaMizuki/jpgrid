#' Zoom-in regional meshes
#'
#' \code{mesh_zoomin} makes the regional meshes finer.
#'
#' @export
#'
#' @inheritParams mesh
#' @inheritParams size
#'
#' @return \code{mesh_zoomin} returns a list of \code{mesh} class vector.
mesh_zoomin <- function(mesh, size) {
  stopifnot(is_mesh(mesh))

  size <- size_match(size)
  ratio <- mesh_size(mesh) / size

  stopifnot(ratio %% 1 == 0)

  n_X <- field(mesh, "n_X")
  n_Y <- field(mesh, "n_Y")

  purrr::map2(n_X, n_Y,
              function(n_X, n_Y) {
                if (!is.na(n_X) && !is.na(n_Y)) {
                  n_X_min <- n_X * ratio
                  n_X_max <- (n_X + 1) * ratio - 1

                  n_Y_min <- n_Y * ratio
                  n_Y_max <- (n_Y + 1) * ratio - 1

                  n_XY <- tidyr::expand_grid(n_X = n_X_min:n_X_max,
                                     n_Y = n_Y_min:n_Y_max)
                  new_mesh(size = size,
                           n_X = n_XY$n_X,
                           n_Y = n_XY$n_Y)
                } else {
                  new_mesh(size = size)
                }
              })
}

#' Zoom-out regional meshes
#'
#' \code{mesh_zoomout} makes the regional meshes coarser.
#'
#' @export
#'
#' @inheritParams mesh
#' @inheritParams size
#'
#' @return \code{mesh_zoomout} returns a \code{mesh} class vector.
mesh_zoomout <- function(mesh, size) {
  stopifnot(is_mesh(mesh))

  size <- size_match(size)
  ratio <- size / mesh_size(mesh)

  stopifnot(ratio %% 1 == 0)

  new_mesh(size = size,
           n_X = field(mesh, "n_X") %/% ratio,
           n_Y = field(mesh, "n_Y") %/% ratio)
}

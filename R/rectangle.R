#' Convert regional meshes into rectangular meshes
#'
#' @inheritParams mesh
#'
#' @return A \code{mesh} vector.
#'
#' @export
mesh_rectangle <- function(mesh, buffer = 0L) {
  n_X <- field(mesh, "n_X")
  n_Y <- field(mesh, "n_Y")
  n_XY <- tidyr::expand_grid(n_X = (min(n_X) - buffer):(max(n_X) + buffer),
                             n_Y = (min(n_Y) - buffer):(max(n_Y) + buffer))
  new_mesh(size = mesh_size(mesh),
           n_X = n_XY$n_X,
           n_Y = n_XY$n_Y)
}

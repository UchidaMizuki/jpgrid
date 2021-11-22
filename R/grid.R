#' Create regional mesh grids
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

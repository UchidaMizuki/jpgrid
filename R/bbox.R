#' Convert grid square codes into bounding codes
#'
#' @inheritParams grid
#'
#' @return A `grid` vector.
#'
#' @export
grid_bbox <- function(grid) {
  n_X <- field(grid, "n_X")
  n_Y <- field(grid, "n_Y")
  n_XY <- tidyr::expand_grid(n_X = min(n_X):max(n_X),
                             n_Y = min(n_Y):max(n_Y))
  new_grid(size = grid_size(grid),
           n_X = n_XY$n_X,
           n_Y = n_XY$n_Y)
}

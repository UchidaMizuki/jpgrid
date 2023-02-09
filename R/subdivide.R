#' Subdivide grid square codes
#'
#' `grid_subdivide()` makes the grid square codes finer.
#'
#' @param grid A `grid` vector.
#' @param grid_size A grid size.
#'
#' @return A list of `grid` vector.
#'
#' @export
grid_subdivide <- function(grid, grid_size) {
  if (!is_grid(grid)) {
    cli_abort("{.arg grid} must be a vector with type {.cls grid}.")
  }

  grid_size <- grid_size_match(grid_size)
  ratio <- grid_size(grid) / grid_size

  if (!is_integerish(ratio)) {
    cli_abort("{.arg grid} can't be subdivided.")
  }
  ratio <- as.integer(ratio)

  n_X <- field(grid, "n_X")
  n_Y <- field(grid, "n_Y")

  purrr::map2(n_X, n_Y,
              \(n_X, n_Y) {
                if (!is.na(n_X) && !is.na(n_Y)) {
                  n_X_min <- n_X * ratio
                  n_X_max <- (n_X + 1L) * ratio - 1L

                  n_Y_min <- n_Y * ratio
                  n_Y_max <- (n_Y + 1L) * ratio - 1L

                  n_XY <- tidyr::expand_grid(n_X = n_X_min:n_X_max,
                                             n_Y = n_Y_min:n_Y_max)
                  new_grid(grid_size = grid_size,
                           n_X = n_XY$n_X,
                           n_Y = n_XY$n_Y)
                } else {
                  new_grid(grid_size = grid_size)
                }
              })
}

#' Subdivide grid square codes
#'
#' `grid_subdivide()` makes the grid square codes finer.
#'
#' @param grid A `grid` vector.
#' @param size A grid size.
#'
#' @return A list of `grid` vector.
#'
#' @export
grid_subdivide <- function(grid, size) {
  stopifnot(is_grid(grid))

  size <- size_match(size)
  ratio <- grid_size(grid) / size

  stopifnot(ratio %% 1L == 0L)
  ratio <- as.integer(ratio)

  n_X <- field(grid, "n_X")
  n_Y <- field(grid, "n_Y")

  purrr::map2(n_X, n_Y,
              function(n_X, n_Y) {
                if (!is.na(n_X) && !is.na(n_Y)) {
                  n_X_min <- n_X * ratio
                  n_X_max <- (n_X + 1L) * ratio - 1L

                  n_Y_min <- n_Y * ratio
                  n_Y_max <- (n_Y + 1L) * ratio - 1L

                  n_XY <- tidyr::expand_grid(n_X = n_X_min:n_X_max,
                                             n_Y = n_Y_min:n_Y_max)
                  new_grid(size = size,
                           n_X = n_XY$n_X,
                           n_Y = n_XY$n_Y)
                } else {
                  new_grid(size = size)
                }
              })
}

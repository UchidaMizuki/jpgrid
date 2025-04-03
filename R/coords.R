#' Conversion between grid square codes and coordinates (longitude and latitude)
#'
#' @name coords
NULL

#' @rdname coords
#'
#' @param X A numeric vector of longitude.
#' @param Y A numeric vector of latitude.
#' @param grid_size A grid size.
#'
#' @return `coords_to_grid()` returns a `grid` vector.
#'
#' @export
coords_to_grid <- function(X, Y, grid_size) {
  grid_size <- grid_size_match(grid_size)

  length_X <- grid_size / 80000L
  length_Y <- length_X / 1.5

  new_grid(
    grid_size = grid_size,
    n_X = (X - 100) %/% length_X,
    n_Y = Y %/% length_Y
  )
}

#' @rdname coords
#'
#' @param grid A `grid` class vector.
#' @param center Should the center point of the grid be returned? Otherwise the
#' end points will be returned. `TRUE` by default.
#'
#' @return `grid_to_coords()` returns a `tbl_df`.
#'
#' @export
grid_to_coords <- function(grid, center = TRUE) {
  if (!is_grid(grid)) {
    cli_abort("{.arg grid} must be a vector with type {.cls grid}.")
  }

  length_X <- grid_size(grid) / 80000L
  length_Y <- length_X / 1.5

  n_X <- field(grid, "n_X")
  n_Y <- field(grid, "n_Y")

  if (center) {
    tibble::tibble(
      X = 100 + length_X * (n_X + .5),
      Y = length_Y * (n_Y + .5)
    )
  } else {
    coords <- tibble::tibble(
      X_min = 100 + length_X * n_X,
      Y_min = length_Y * n_Y
    )
    coords$X_max <- coords$X_min + length_X
    coords$Y_max <- coords$Y_min + length_Y
    coords
  }
}

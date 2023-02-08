#' Conversion between grid square codes and coordinates (longitude and latitude)
#'
#' @name XY
NULL

#' @rdname XY
#'
#' @param X A numeric vector of longitude.
#' @param Y A numeric vector of latitude.
#' @param size A grid size.
#'
#' @return `grid_from_XY` returns a `grid` vector.
#'
#' @export
grid_from_XY <- function(X, Y, size) {
  size <- grid_size_match(size)

  length_X <- size / 80000L
  length_Y <- length_X / 1.5

  new_grid(size = size,
           n_X = (X - 100) %/% length_X,
           n_Y = Y %/% length_Y)
}

#' @rdname XY
#'
#' @param grid A `grid` class vector.
#' @param center Should the center point of the grid be returned? Otherwise the
#' end points will be returned. `TRUE` by default.
#'
#' @return `grid_to_XY` returns a `tbl_df`.
#'
#' @export
grid_to_XY <- function(grid, center = TRUE) {
  stopifnot(is_grid(grid))

  length_X <- grid_size(grid) / 80000L
  length_Y <- length_X / 1.5

  n_X <- field(grid, "n_X")
  n_Y <- field(grid, "n_Y")

  if (center) {
    tibble::tibble(X = 100 + length_X * (n_X + .5),
                   Y = length_Y * (n_Y + .5))
  } else {
    XY <- tibble::tibble(X_min = 100 + length_X * n_X,
                         Y_min = length_Y * n_Y)
    XY$X_max <- XY$X_min + length_X
    XY$Y_max <- XY$Y_min + length_Y
    XY
  }
}

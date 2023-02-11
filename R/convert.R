#' Convert the grid size of grid objects
#'
#' @param grid A `grid` vector.
#' @param grid_size A grid size.
#'
#' @return A `grid` vector.
#'
#' @examples
#' grid_500m <- parse_grid(c("533945263", "533935863", "533945764"), "500m")
#' grid_convert(grid_500m, "10km")
#'
#' @export
grid_convert <- function(grid, grid_size) {
  if (!is_grid(grid)) {
    cli_abort("{.arg grid} must be a vector with type {.cls grid}.")
  }

  grid_size <- grid_size_match(grid_size)
  ratio <- grid_size / grid_size(grid)

  if (!is_integerish(ratio)) {
    cli_abort(c("{.arg grid} must be able to convert one-to-one.",
                "i" = "You can use {.fn grid_subdivide}."))
  }
  ratio <- as.integer(ratio)

  new_grid(grid_size = grid_size,
           n_X = field(grid, "n_X") %/% ratio,
           n_Y = field(grid, "n_Y") %/% ratio)
}

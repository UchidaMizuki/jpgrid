#' Circular grid square codes
#'
#' @param X A numeric vector of longitude.
#' @param Y A numeric vector of latitude.
#' @param dist A numeric vector of distances passed on to [sf::st_buffer()].
#' @param grid_size A grid size.
#' @param crs Coordinate reference system.
#' @param ... Passed on to `geometry_to_grid()`.
#'
#' @return A list of `grid` vector.
#'
#' @export
grid_circle <- function(X, Y, dist, grid_size,
                        crs = 4326, ...) {
  tibble::tibble(X = X,
                 Y = Y) |>
    sf::st_as_sf(coords = c("X", "Y"),
                 crs = crs) |>
    sf::st_buffer(dist) |>
    geometry_to_grid(grid_size = grid_size, ...)
}

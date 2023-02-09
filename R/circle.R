#' Circular grid square codes
#'
#' @param X A numeric vector of longitude.
#' @param Y A numeric vector of latitude.
#' @param dist A numeric vector of distances passed on to [sf::st_buffer()].
#' @param size A grid size.
#' @param crs Coordinate reference system.
#' @param ... Passed on to `grid_from_geom()`.
#'
#' @return A list of `grid` vector.
#'
#' @export
grid_circle <- function(X, Y, dist, size,
                        crs = 4326, ...) {
  tibble::tibble(X = X,
                 Y = Y) |>
    sf::st_as_sf(coords = c("X", "Y"),
                 crs = crs) |>
    sf::st_buffer(dist) |>
    grid_from_geom(size = size, ...)
}

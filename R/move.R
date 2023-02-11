#' Moving on grid square codes
#'
#' @param grid A `grid` vector.
#' @param n_X Number of moving cells in the longitude direction.
#' @param n_Y Number of moving cells in the latitude direction.
#'
#' @return A `grid` vector.
#'
#' @export
grid_move <- function(grid, n_X, n_Y) {
  if (!is_grid(grid)) {
    cli_abort("{.arg grid} must be a vector with type {.cls grid}.")
  }

  new_grid(grid_size = grid_size(grid),
           n_X = field(grid, "n_X") + n_X,
           n_Y = field(grid, "n_Y") + n_Y)
}

#' Neighborhood grid square codes
#'
#' @param grid A `grid` vector.
#' @param n A numeric vector of degrees.
#' @param moore Moore neighborhood (`TRUE`) or Von Neumann neighborhood
#' (`FALSE`).
#' @param simplify Should simplify the format of the return?
#'
#' @return A list of `grid` vectors.
#'
#' @export
grid_neighbor <- function(grid,
                          n = 1L,
                          moore = TRUE,
                          simplify = TRUE) {
  if (!all(n >= 0 & is_integerish(n))) {
    cli_abort("{.arg n} must be a {.cls integer} vector greater than or equal to 0.")
  }

  n_XY <- n |>
    purrr::map_dfr(function(n) {
      n_XY <- tidyr::expand_grid(n = n,
                                 n_X = -n:n,
                                 n_Y = -n:n)
      vec_slice(n_XY,
                (!moore | abs(n_XY$n_X) == n | abs(n_XY$n_Y) == n) &
                  (moore | (abs(n_XY$n_X) + abs(n_XY$n_Y)) == n))
    })

  neighbor <- tibble::tibble(grid = grid) |>
    vec_unique() |>
    tidyr::expand_grid(n_XY)
  neighbor$grid_neighbor <- neighbor$grid |>
    grid_move(n_X = neighbor$n_X,
              n_Y = neighbor$n_Y)
  neighbor <- neighbor |>
    dplyr::group_nest(grid,
                      .key = "neighbor")

  if (simplify) {
    neighbor$neighbor <- neighbor$neighbor |>
      purrr::map(function(neighbor) {
        neighbor |>
          purrr::chuck("grid_neighbor")
      })
  }

  tibble::tibble(grid = grid) |>
    dplyr::left_join(neighbor,
                     by = "grid") |>
    purrr::chuck("neighbor")
}

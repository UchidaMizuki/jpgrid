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
#' @param n A numeric vector of degrees. By default, `1L`.
#' @param moore Moore neighborhood (`TRUE`, default) or Von Neumann neighborhood
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
    tidyr::nest(.by = "grid",
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

#' Connected components of grid square codes
#'
#' @param grid A `grid` vector.
#' @param n A numeric vector of degrees. By default, `0:1`.
#' @param moore Moore neighborhood (`TRUE`) or Von Neumann neighborhood
#' (`FALSE`, default).
#'
#' @return A integer vector of group IDs.
#'
#' @export
grid_components <- function(grid,
                            n = 0:1,
                            moore = FALSE) {
  edges <- tibble::tibble(grid_from = grid,
                          grid_to = grid_neighbor(grid,
                                                  n = n,
                                                  moore = moore)) |>
    tidyr::unnest("grid_to") |>
    dplyr::filter(.data$grid_to %in% grid)

  grid_unique <- vec_unique(c(edges$grid_from, edges$grid_to))

  edges <- edges |>
    dplyr::mutate(grid_from = vec_match(.data$grid_from, grid_unique),
                  grid_to = vec_match(.data$grid_to, grid_unique))

  group <- tidygraph::tbl_graph(edges = edges) |>
    dplyr::mutate(group = tidygraph::group_components()) |>
    dplyr::pull("group")

  vec_slice(group, vec_match(grid, grid_unique))
}

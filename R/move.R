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
#' @param type A character vector of neighborhood types, `"von_neumann"` or
#' `"moore"`. By default, `"von_neumann"`.
#' @param simplify Should simplify the format of the return?
#'
#' @return A list of `grid` vectors.
#'
#' @export
grid_neighborhood <- function(grid,
                              n = 1L,
                              type = NULL,
                              simplify = TRUE) {
  if (is.null(type)) {
    type <- "von_neumann"
    cli_inform(c('Using default neighborhood type "von_neumann".',
                 "i" = 'Use `type = "moore"` for Moore neighborhood.'))
  }

  type <- arg_match(type, c("von_neumann", "moore"))

  if (!all(n >= 0 & is_integerish(n))) {
    cli_abort("{.arg n} must be a {.cls integer} vector greater than or equal to 0.")
  }

  n_XY <- n |>
    purrr::map(\(n) {
      n_XY <- tidyr::expand_grid(n = n,
                                 n_X = -n:n,
                                 n_Y = -n:n)
      vec_slice(n_XY,
                (type != "von_neumann" | (abs(n_XY$n_X) + abs(n_XY$n_Y)) == n) &
                  (type != "moore" | abs(n_XY$n_X) == n | abs(n_XY$n_Y) == n))
    }) |>
    purrr::list_rbind()

  neighbor <- tibble::tibble(grid = grid) |>
    vec_unique() |>
    tidyr::expand_grid(n_XY)
  neighbor$grid_neighborhood <- neighbor$grid |>
    grid_move(n_X = neighbor$n_X,
              n_Y = neighbor$n_Y)
  neighbor <- neighbor |>
    tidyr::nest(.by = "grid",
                .key = "neighbor")

  if (simplify) {
    neighbor$neighbor <- neighbor$neighbor |>
      purrr::map(\(neighbor) {
        neighbor |>
          purrr::chuck("grid_neighborhood")
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
#' @param type A character vector of neighborhood types, `"von_neumann"` or
#' `"moore"`. By default, `"von_neumann"`.
#' (`FALSE`, default).
#'
#' @return A integer vector of group IDs.
#'
#' @export
grid_components <- function(grid,
                            n = 0:1,
                            type = NULL) {
  grid_unique <- vec_unique(grid)

  nodes <- tibble::tibble(grid = grid_unique)
  edges <- tibble::tibble(grid_from = grid_unique,
                          grid_to = grid_neighborhood(grid_unique,
                                                      n = n,
                                                      type = type)) |>
    tidyr::unnest("grid_to") |>
    dplyr::filter(.data$grid_to %in% grid_unique) |>
    dplyr::mutate(grid_from = vec_match(.data$grid_from, grid_unique),
                  grid_to = vec_match(.data$grid_to, grid_unique))

  group_nodes <- tidygraph::tbl_graph(nodes = nodes,
                                      edges = edges) |>
    dplyr::mutate(group = tidygraph::group_components()) |>
    tibble::as_tibble()

  vec_slice(group_nodes$group, vec_match(grid, group_nodes$grid))
}

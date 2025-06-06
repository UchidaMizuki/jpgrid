#' Distance between grid square codes
#'
#' If `grid` and `grid_to` are both vectors, the distance between
#' `grid` and `grid_to` is calculated.
#' If `grid` is a list, The path distance of each element is calculated.
#'
#' @param grid A `grid` vector or a list of `grid` vector.
#' @param grid_to A `grid` vector.
#' @param close Should the path of each element be closed when `grid` is a list?
#' @param type How is the NA grid treated when `grid` is a list?
#' `"skip_na"` skips the `NA` grid and connects the paths.
#' `"keep_na"` by default.
#'
#' @return A double vector.
#'
#' @export
grid_distance <- function(
  grid,
  grid_to = NULL,
  close = FALSE,
  type = c("keep_na", "ignore_na", "skip_na")
) {
  if (is_grid(grid)) {
    if (!is_grid(grid_to)) {
      cli_abort("{.arg grid_to} must be a vector with type {.cls grid}.")
    }

    grid_size <- grid_size(grid)
    if (grid_size != grid_size(grid_to)) {
      cli_abort(
        "The grid size of {.arg grid} and {.arg grid_to} must be the same."
      )
    }

    grid <- tibble::tibble(
      diff_n_X = field(grid_to, "n_X") - field(grid, "n_X"),
      n_Y = field(grid, "n_Y"),
      n_Y_to = field(grid_to, "n_Y")
    )

    length_X <- grid_size / 80000L
    length_Y <- length_X / 1.5

    distance <- vec_unique(grid)
    distance <- vec_slice(
      distance,
      !is.na(distance$diff_n_X) &
        !is.na(distance$n_Y) &
        !is.na(distance$n_Y_to)
    )

    diff_X <- length_X * distance$diff_n_X
    Y <- length_Y * (distance$n_Y + .5)
    Y_to <- length_Y * (distance$n_Y_to + .5)

    distance$distance <- distance_by_element(
      X_from = 0,
      Y_from = Y,
      X_to = diff_X,
      Y_to = Y_to
    )

    grid |>
      dplyr::left_join(distance, by = c("diff_n_X", "n_Y", "n_Y_to")) |>
      purrr::chuck("distance")
  } else {
    if (!is.list(grid)) {
      cli_abort("{.arg grid} must be a {.cls list}.")
    }
    if (!is.null(grid_to)) {
      cli_abort(
        "If {.arg grid} is a {.cls list}, {.arg grid_to} must be {.var NULL}."
      )
    }
    type <- arg_match(type, c("keep_na", "ignore_na", "skip_na"))

    grid |>
      purrr::modify(\(grid) {
        if (type == "skip_na") {
          grid <- grid |>
            vec_slice(!is.na(grid))
        }

        if (close) {
          grid_to <- c(utils::tail(grid, -1L), grid[1L])
        } else {
          grid_to <- utils::tail(grid, -1L)
          grid <- utils::head(grid, -1L)
        }

        grid_distance(grid, grid_to) |>
          sum(na.rm = type == "ignore_na")
      })
  }
}

distance_by_element <- function(X_from, Y_from, X_to, Y_to) {
  from <- tibble::tibble(
    X_from = X_from,
    Y_from = Y_from
  ) |>
    sf::st_as_sf(coords = c("X_from", "Y_from"), crs = 4326)
  to <- tibble::tibble(
    X_to = X_to,
    Y_to = Y_to
  ) |>
    sf::st_as_sf(coords = c("X_to", "Y_to"), crs = 4326)
  sf::st_distance(from, to, by_element = TRUE)
}

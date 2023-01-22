#' Draw line segments between grid square codes
#'
#' If `grid` and `grid_to` are both vectors, the line between
#' `grid` and `grid_to` is drawn (using Bresenham's line algorithm).
#' If `grid` is a list, The path lines for each element in the grid will be
#' drawn.
#'
#' @inheritParams grid_to
#' @param skip_na Should skip the `NA` grid and connects the paths? `FALSE` by
#' default.
#'
#' @return A list of `grid` vectors.
#'
#' @export
grid_line <- function(grid, grid_to,
                      close = FALSE,
                      skip_na = FALSE) {
  if (is_grid(grid)) {
    stopifnot(is_grid(grid_to))

    size <- grid_size(grid)
    stopifnot(size == grid_size(grid_to))

    grid <- tibble::tibble(grid = grid,
                           grid_to = grid_to)

    line <- vec_unique(grid)
    line <- vec_slice(line,
                      !is.na(line$grid) &
                        !is.na(line$grid_to))

    # Bresenham's line algorithm
    x <- field(line$grid, "n_X")
    y <- field(line$grid, "n_Y")

    x_to <- field(line$grid_to, "n_X")
    y_to <- field(line$grid_to, "n_Y")

    dx <- abs(x_to - x)
    dy <- abs(y_to - y)
    err <- dx - dy

    sx <- dplyr::if_else(x < x_to, 1L, -1L)
    sy <- dplyr::if_else(y < y_to, 1L, -1L)

    line$line <- list(x, y, x_to, y_to, dx, dy, err, sx, sy) |>
      purrr::pmap(function(x, y, x_to, y_to, dx, dy, err, sx, sy) {
        if (is.na(x) || is.na(y) || is.na(x_to) || is.na(y_to)) {
          new_grid(size = size,
                   n_X = NA_integer_,
                   n_Y = NA_integer_)
        } else {
          xs <- x
          ys <- y

          while (x != x_to || y != y_to) {
            err_2 <- err * 2L
            if (err_2 >= -dy) {
              err <- err - dy
              x <- x + sx
            }
            if (err_2 <= dx) {
              err <- err + dx
              y <- y + sy
            }
            xs <- c(xs, x)
            ys <- c(ys, y)
          }
          new_grid(size = size,
                   n_X = xs,
                   n_Y = ys)
        }
      })

    grid |>
      dplyr::left_join(line,
                       by = c("grid", "grid_to")) |>
      purrr::chuck("line")
  } else {
    stopifnot(is.list(grid),
              missing(grid_to))

    grid |>
      purrr::modify(function(grid) {
        if (skip_na) {
          grid <- grid |>
            vec_slice(!is.na(grid))
        }

        if (close) {
          grid_to <- c(utils::tail(grid, -1L), grid[1L])
        } else {
          grid_to <- utils::tail(grid, -1L)
          grid <- utils::head(grid, -1L)
        }

        grid_line(grid, grid_to) |>
          purrr::reduce(c)
      })
  }
}

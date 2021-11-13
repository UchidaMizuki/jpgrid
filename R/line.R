#' @export
mesh_line <- function(mesh_start, mesh_end) {
  stopifnot(is_mesh(mesh_start),
            is_mesh(mesh_end),
            mesh_size(mesh_start) == mesh_size(mesh_end))

  size <- mesh_size(mesh_start)

  # Bresenham's line algorithm
  x <- field(mesh_start, "n_X")
  y <- field(mesh_start, "n_Y")

  x_end <- field(mesh_end, "n_X")
  y_end <- field(mesh_end, "n_Y")

  dx <- abs(x_end - x)
  dy <- abs(y_end - y)
  err <- dx - dy

  sx <- dplyr::if_else(x < x_end, 1, -1)
  sy <- dplyr::if_else(y < y_end, 1, -1)

  list(x, y, x_end, y_end, dx, dy, err, sx, sy) %>%
    purrr::pmap(function(x, y, x_end, y_end, dx, dy, err, sx, sy) {
      xs <- x
      ys <- y

      while (x != x_end || y != y_end) {
        err_2 <- err * 2
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
      mesh(n_X = xs,
           n_Y = ys,
           size = size)
    })
}

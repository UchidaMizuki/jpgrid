#' @export
mesh_line <- function(mesh, mesh_end,
                      skip_na = F,
                      close = F) {
  if (is_mesh(mesh)) {
    stopifnot(is_mesh(mesh_end))

    size <- mesh_size(mesh)
    stopifnot(size == mesh_size(mesh_end))

    # Bresenham's line algorithm
    x <- field(mesh, "n_X")
    y <- field(mesh, "n_Y")

    x_end <- field(mesh_end, "n_X")
    y_end <- field(mesh_end, "n_Y")

    dx <- abs(x_end - x)
    dy <- abs(y_end - y)
    err <- dx - dy

    sx <- dplyr::if_else(x < x_end, 1, -1)
    sy <- dplyr::if_else(y < y_end, 1, -1)

    list(x, y, x_end, y_end, dx, dy, err, sx, sy) %>%
      purrr::pmap(function(x, y, x_end, y_end, dx, dy, err, sx, sy) {
        if (is_na(x) || is_na(y) || is_na(x_end) || is_na(y_end)) {
          mesh(n_X = NA_integer_,
               n_Y = NA_integer_,
               size = size)
        } else {
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
        }
      }) %>%
      as_list_of(.type = mesh)
  } else {
    stopifnot(is_list(mesh),
              missing(mesh_end))

    mesh %>%
      purrr::modify(function(mesh) {
        if (skip_na) {
          mesh <- mesh %>%
            purrr::discard(are_na)
        }

        if (close) {
          mesh_end <- c(tail(mesh, -1), mesh[1])
        } else {
          mesh_end <- tail(mesh, -1)
          mesh <- head(mesh, -1)
        }

        mesh_line(mesh, mesh_end) %>%
          purrr::reduce(c)
      })
  }
}

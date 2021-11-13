# FIXME?
#' @export
mesh_line <- function(mesh, mesh_end,
                      skip_na = F,
                      close = F) {
  if (is_mesh(mesh)) {
    stopifnot(is_mesh(mesh_end))

    size <- mesh_size(mesh)
    stopifnot(size == mesh_size(mesh_end))

    mesh <- data_frame(mesh = mesh,
                       mesh_end = mesh_end)

    mesh_unique <- mesh %>%
      vec_unique() %>%
      tidyr::drop_na(mesh, mesh_end)

    # Bresenham's line algorithm
    x <- field(mesh_unique$mesh, "n_X")
    y <- field(mesh_unique$mesh, "n_Y")

    x_end <- field(mesh_unique$mesh_end, "n_X")
    y_end <- field(mesh_unique$mesh_end, "n_Y")

    dx <- abs(x_end - x)
    dy <- abs(y_end - y)
    err <- dx - dy

    sx <- dplyr::if_else(x < x_end, 1, -1)
    sy <- dplyr::if_else(y < y_end, 1, -1)

    line <- list(x, y, x_end, y_end, dx, dy, err, sx, sy) %>%
      purrr::pmap(function(x, y, x_end, y_end, dx, dy, err, sx, sy) {
        if (is.na(x) || is.na(y) || is.na(x_end) || is.na(y_end)) {
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

    mesh_unique <- mesh_unique %>%
      tibble::add_column(line = line)

    mesh %>%
      dplyr::left_join(mesh_unique,
                       by = c("mesh", "mesh_end")) %>%
      purrr::chuck("line")
  } else {
    stopifnot(is_list(mesh),
              missing(mesh_end))

    mesh %>%
      purrr::modify(function(mesh) {
        if (skip_na) {
          mesh <- mesh %>%
            vec_slice(!is.na(mesh))
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

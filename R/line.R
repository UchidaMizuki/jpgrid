#' Draw line segments between meshes
#'
#' If \code{mesh} and \code{mesh_to} are both vectors, the line between \code{mesh} and \code{mesh_to} is drawn (using Bresenham's line algorithm).
#' If \code{mesh} is a list, The path lines for each element in the mesh will be drawn.
#'
#' @inheritParams mesh_to
#' @param skip_na Should skip the \code{NA} mesh and connects the paths? \code{FALSE} by default.
#'
#' @export
mesh_line <- function(mesh, mesh_to,
                      close = FALSE,
                      skip_na = FALSE) {
  if (is_mesh(mesh)) {
    stopifnot(is_mesh(mesh_to))

    size <- mesh_size(mesh)
    stopifnot(size == mesh_size(mesh_to))

    mesh <- tibble::tibble(mesh = mesh,
                           mesh_to = mesh_to)

    line <- mesh %>%
      vec_unique() %>%
      dplyr::filter(!is.na(mesh),
                    !is.na(mesh_to)) %>%

      # Bresenham's line algorithm
      dplyr::mutate(x = field(mesh, "n_X"),
                    y = field(mesh, "n_Y"),

                    x_to = field(mesh_to, "n_X"),
                    y_to = field(mesh_to, "n_Y"),

                    dx = abs(x_to - x),
                    dy = abs(y_to - y),
                    err = dx - dy,

                    sx = dplyr::if_else(x < x_to,
                                        1,
                                        -1),
                    sy = dplyr::if_else(y < y_to,
                                        1,
                                        -1)) %>%
      dplyr::mutate(line = list(x, y, x_to, y_to, dx, dy, err, sx, sy) %>%
                      purrr::pmap(function(x, y, x_to, y_to, dx, dy, err, sx, sy) {
                        if (is.na(x) || is.na(y) || is.na(x_to) || is.na(y_to)) {
                          new_mesh(n_X = NA_integer_,
                                   n_Y = NA_integer_,
                                   size = size)
                        } else {
                          xs <- x
                          ys <- y

                          while (x != x_to || y != y_to) {
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
                          new_mesh(n_X = xs,
                                   n_Y = ys,
                                   size = size)
                        }
                      })) %>%
      dplyr::select(mesh, mesh_to, line)

    mesh %>%
      dplyr::left_join(line,
                       by = c("mesh", "mesh_to")) %>%
      purrr::chuck("line")
  } else {
    stopifnot(is_list(mesh),
              missing(mesh_to))

    mesh %>%
      purrr::modify(function(mesh) {
        if (skip_na) {
          mesh <- mesh %>%
            vec_slice(!is.na(mesh))
        }

        if (close) {
          mesh_to <- c(utils::tail(mesh, -1), mesh[1])
        } else {
          mesh_to <- utils::tail(mesh, -1)
          mesh <- utils::head(mesh, -1)
        }

        mesh_line(mesh, mesh_to) %>%
          purrr::reduce(c)
      })
  }
}

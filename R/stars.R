#' Converting data frame containing regional meshes to stars
#'
#' @param x A data frame.
#' @param mesh_column_name A scalar character.
#' @param ... passed on to \code{stars::st_as_stars()}.
#'
#' @return A \code{stars} object.
#'
#' @export
mesh_as_stars <- function(x,
                          mesh_column_name = NULL,
                          ...) {
  stopifnot(is.data.frame(x))

  if (is.null(mesh_column_name)) {
    i <- x %>%
      purrr::map_lgl(is_mesh)
    mesh_column_name <- names(x) %>%
      vec_slice(i) %>%
      vec_slice(1L)
  }
  mesh <- x[[mesh_column_name]]

  mesh <- mesh_rectangle(mesh)
  x <- tibble::tibble(!!mesh_column_name := mesh) %>%
    dplyr::left_join(x,
                     by = mesh_column_name)
  XY <- mesh_to_XY(x[[mesh_column_name]])
  x$X <- XY$X
  x$Y <- XY$Y
  x <- x[names(x) != mesh_column_name]

  stars::st_as_stars(x,
                     coords = c("X", "Y"),
                     ...)
}

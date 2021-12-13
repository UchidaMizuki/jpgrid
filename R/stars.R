#' Converting data frame containing regional meshes to stars
#'
#' @param x A data frame.
#' @param mesh_column_name A scalar character.
#' @param ... Passed on to \code{stars::st_as_stars()}.
#'
#' @return A \code{stars} object.
#'
#' @export
mesh_as_stars <- function(x,
                          coords = NULL,
                          crs = sf::NA_crs_,
                          mesh_column_name = NULL,
                          ...) {
  if (is_mesh(x)) {
    x <- tibble::tibble(mesh = x,
                        values = 0)
  }
  stopifnot(is.data.frame(x))

  if (is.null(mesh_column_name)) {
    i <- x %>%
      purrr::map_lgl(is_mesh)
    mesh_column_name <- names(x) %>%
      vec_slice(i) %>%
      vec_slice(1L)
  }
  mesh <- x[[mesh_column_name]]

  n_X <- field(mesh, "n_X")
  n_Y <- field(mesh, "n_Y")
  n_XY <- tidyr::expand_grid(n_X = min(n_X):(max(n_X) + 1L),
                             n_Y = min(n_Y):(max(n_Y) + 1L))
  mesh <- new_mesh(size = mesh_size(mesh),
                   n_X = n_XY$n_X,
                   n_Y = n_XY$n_Y)
  XY <- mesh_to_XY(mesh)
  mesh <- tibble::tibble(!!mesh_column_name := mesh,
                         X = XY$X,
                         Y = XY$Y)

  coords <- coords[coords != mesh_column_name]
  x <- tidyr::expand_grid(mesh,
                          vctrs::vec_unique(x[coords])) %>%
    dplyr::left_join(x,
                     by = c(mesh_column_name, coords))
  x <- x[names(x) != mesh_column_name]

  x <- stars::st_as_stars(x,
                          coords = c("X", "Y", coords),
                          y_decreasing = FALSE,
                          ...) %>%
    sf::st_set_crs(crs)
  dim_x <- dim(x)
  x %>%
    dplyr::slice("X", 1L:(dim_x[["X"]] - 1L),
                 drop = FALSE) %>%
    dplyr::slice("Y", 1L:(dim_x[["Y"]] - 1L),
                 drop = FALSE)
}

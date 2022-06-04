#' Converting data frame containing regional grids to stars
#'
#' @param x A data frame or a `grid`.
#' @param coords The column names or indices that form the cube dimensions.
#' @param crs Coordinate reference system.
#' @param grid_column_name A scalar character.
#' @param ... Passed on to [stars::st_as_stars()].
#'
#' @return A \code{stars} object.
#'
#' @export
grid_as_stars <- function(x,
                          coords = NULL,
                          crs = sf::NA_crs_,
                          grid_column_name = NULL, ...) {
  if (is_grid(x)) {
    x <- tibble::tibble(grid = x,
                        values = NA_real_)
    grid_column_name <- "grid"
  }
  stopifnot(is.data.frame(x))

  if (is.null(grid_column_name)) {
    i <- x %>%
      purrr::map_lgl(is_grid)
    grid_column_name <- names(x) %>%
      vec_slice(i) %>%
      vec_slice(1L)
  }
  grid <- x[[grid_column_name]]

  n_X <- field(grid, "n_X")
  n_Y <- field(grid, "n_Y")
  n_XY <- tidyr::expand_grid(n_X = min(n_X):(max(n_X) + 1L),
                             n_Y = min(n_Y):(max(n_Y) + 1L))
  grid <- new_grid(size = grid_size(grid),
                   n_X = n_XY$n_X,
                   n_Y = n_XY$n_Y)
  XY <- grid_to_XY(grid)
  grid <- tibble::tibble(!!grid_column_name := grid,
                         X = XY$X,
                         Y = XY$Y)

  coords <- coords[coords != grid_column_name]
  x <- tidyr::expand_grid(grid,
                          vctrs::vec_unique(x[coords])) %>%
    dplyr::left_join(x,
                     by = c(grid_column_name, coords))
  x <- x[names(x) != grid_column_name]

  x <- stars::st_as_stars(x,
                          coords = c("X", "Y", coords),
                          y_decreasing = FALSE, ...) %>%
    sf::st_set_crs(crs)
  dim_x <- dim(x)
  x %>%
    dplyr::slice("X", 1L:(dim_x[["X"]] - 1L),
                 drop = FALSE) %>%
    dplyr::slice("Y", 1L:(dim_x[["Y"]] - 1L),
                 drop = FALSE)
}

#' @importFrom stars st_as_stars
#' @export
st_as_stars.grid <- function(.x, ...) {
  grid_as_stars(.x)
}

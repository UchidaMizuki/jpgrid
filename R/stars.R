#' @importFrom stars st_as_stars
#' @export
st_as_stars.grid <- function(.x, ...) {
  tibble::tibble(grid = .x,
                 values = NA_real_) %>%
    as_tbl_grid() |>
    st_as_stars(...)
}

#' @export
st_as_stars.tbl_grid <- function(.x,
                                 coords = NULL,
                                 crs = sf::NA_crs_, ...) {
  grid_col <- grid_column(.x)
  grid <- .x[[grid_col]]

  n_X <- field(grid, "n_X")
  n_Y <- field(grid, "n_Y")
  n_XY <- tidyr::expand_grid(n_X = min(n_X):(max(n_X) + 1L),
                             n_Y = min(n_Y):(max(n_Y) + 1L))
  grid <- new_grid(size = grid_size(grid),
                   n_X = n_XY$n_X,
                   n_Y = n_XY$n_Y)
  XY <- grid_to_XY(grid)
  grid <- tibble::tibble(!!grid_col := grid,
                         X = XY$X,
                         Y = XY$Y)

  coords <- coords[coords != grid_col]
  .x <- tidyr::expand_grid(grid,
                           vctrs::vec_unique(.x[coords])) %>%
    dplyr::left_join(.x,
                     by = c(grid_col, coords))
  .x <- .x[names(.x) != grid_col]

  .x <- stars::st_as_stars(.x,
                           coords = c("X", "Y", coords),
                           y_decreasing = FALSE, ...) %>%
    sf::st_set_crs(crs)
  dim_x <- dim(.x)
  .x %>%
    dplyr::slice("X", 1L:(dim_x[["X"]] - 1L),
                 drop = FALSE) %>%
    dplyr::slice("Y", 1L:(dim_x[["Y"]] - 1L),
                 drop = FALSE)
}

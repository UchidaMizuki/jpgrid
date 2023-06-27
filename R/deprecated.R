#' Grid square code vector
#'
#' `r lifecycle::badge("deprecated")`
#'
#' It is recommended to use `grid_parse()` or `grid_convert()`.
#'
#' A series of functions return `grid` class for each grid size.
#' `grid_auto()` returns automatically determine grid size by the largest
#' grid size.
#'
#' @name grid_class
#'
#' @param x A list or vector.
#' @param strict A scalar logical. Should the number of digits in the grid
#' square code match a given number of digits?
#'
#' @return A `grid` vector.
NULL

grid_impl <- function(x, strict, grid_size, what) {
  lifecycle::deprecate_warn("0.4.0", stringr::str_c(what, "()"),
                            details = "Please use `parse_grid()` or `grid_convert()`")

  if (is_grid(x)) {
    grid_convert(x,
                 grid_size = grid_size)
  } else {
    parse_grid(x,
               grid_size = grid_size,
               strict = strict)
  }
}

#' @export
#' @rdname grid_class
grid_80km <- function(x,
                      strict = TRUE) {
  grid_impl(x,
            strict = strict,
            grid_size = 80000L,
            what = "grid_80km")
}

#' @export
#' @rdname grid_class
grid_10km <- function(x,
                      strict = TRUE) {
  grid_impl(x,
            strict = strict,
            grid_size = 10000L,
            what = "grid_10km")
}

#' @export
#' @rdname grid_class
grid_1km <- function(x,
                     strict = TRUE) {
  grid_impl(x,
            strict = strict,
            grid_size = 1000L,
            what = "grid_1km")
}

#' @export
#' @rdname grid_class
grid_500m <- function(x,
                      strict = TRUE) {
  grid_impl(x,
            strict = strict,
            grid_size = 500L,
            what = "grid_500m")
}

#' @export
#' @rdname grid_class
grid_250m <- function(x,
                      strict = TRUE) {
  grid_impl(x,
            strict = strict,
            grid_size = 250L,
            what = "grid_250m")
}

#' @export
#' @rdname grid_class
grid_125m <- function(x,
                      strict = TRUE) {
  grid_impl(x,
            strict = strict,
            grid_size = 125L,
            what = "grid_125m")
}

#' @export
#' @rdname grid_class
grid_100m <- function(x,
                      strict = TRUE) {
  grid_impl(x,
            strict = strict,
            grid_size = 100L,
            what = "grid_100m")
}

#' @export
#' @rdname grid_class
grid_auto <- function(x,
                      strict = TRUE) {
  grid_impl(x,
            strict = strict,
            grid_size = NULL,
            what = "grid_auto")
}

#' Conversion between grid square codes and coordinates (longitude and latitude)
#'
#' `r lifecycle::badge("deprecated")`
#'
#' @name XY
NULL

#' @rdname XY
#'
#' @param grid A `grid` class vector.
#' @param center Should the center point of the grid be returned? Otherwise the
#' end points will be returned. `TRUE` by default.
#'
#' @return `grid_to_XY()` returns a `tbl_df`.
#'
#' @export
grid_to_XY <- function(grid, center = TRUE) {
  lifecycle::deprecate_warn("0.4.0", "grid_to_XY()", "grid_to_coords()")
  grid_to_coords(grid = grid,
                 center = center)
}

#' @rdname XY
#'
#' @param X A numeric vector of longitude.
#' @param Y A numeric vector of latitude.
#' @param grid_size A grid size.
#'
#' @return `XY_to_grid()` returns a `grid` vector.
#'
#' @export
XY_to_grid <- function(X, Y, grid_size) {
  lifecycle::deprecate_warn("0.4.0", "XY_to_grid()", "coords_to_grid()")
  coords_to_grid(X = X,
                 Y = Y,
                 grid_size = grid_size)
}

#' Convert a data frame into a tbl_grid object
#'
#' `r lifecycle::badge("deprecated")`
#'
#' It is recommended to use `grid_as_sf()`.
#'
#' The `tbl_grid` object is a data frame with `grid` objects in the columns.
#' `as_tbl_grid` converts a data frame into a tbl_grid object.
#'
#' @param x An object to be converted into an object class `tbl_grid`.
#' @param var A variable to specify the grid object. By default, the first
#' column of the grid object is taken.
#' @param grid_size 	A grid size.
#' @param strict A scalar logical. Should the number of digits in the grid
#' square code match a given number of digits?
#' @param ... Additional arguments passed to [tibble::new_tibble()].
#'
#' @return A `tbl_grid` object.
#'
#' @export
as_tbl_grid <- function(x,
                        var = NULL,
                        grid_size = NULL,
                        strict = TRUE, ...) {
  lifecycle::deprecate_warn("0.4.0", "as_tbl_grid()", "grid_as_sf()")

  if (quo_is_null(enquo(var))) {
    are_grid <- purrr::map_lgl(x, is_grid)

    if (any(are_grid)) {
      var <- names(x)[[which(are_grid)[[1L]]]]
    } else {
      abort("`x` must have a column of `grid` class.")
    }
  } else {
    var <- tidyselect::vars_pull(names(x), {{ var }})
  }

  out <- x |>
    dplyr::mutate(dplyr::across(dplyr::all_of(var),
                                purrr::partial(parse_grid,
                                               strict = strict,
                                               grid_size = grid_size)))

  tibble::new_tibble(out,
                     grid_col = var, ...,
                     class = "tbl_grid")
}

grid_column <- function(x) {
  attr(x, "grid_col")
}

#' @importFrom sf st_as_sf
#' @export
st_as_sf.tbl_grid <- function(x,
                              as_points = FALSE,
                              crs = sf::NA_crs_, ...) {
  grid <- x[[grid_column(x)]]

  x |>
    tibble::as_tibble() |>
    sf::st_set_geometry(grid |>
                          st_as_sfc(as_points = as_points,
                                    crs = crs)) |>
    sf::st_as_sf(...)
}


#' @export
st_bbox.tbl_grid <- function(obj, ...) {
  obj <- obj[[grid_column(obj)]]
  st_bbox(obj)
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
  grid <- new_grid(grid_size = grid_size(grid),
                   n_X = n_XY$n_X,
                   n_Y = n_XY$n_Y)
  coords <- grid_to_coords(grid)
  grid <- tibble::tibble(!!grid_col := grid,
                         X = coords$X,
                         Y = coords$Y)

  coords <- coords[coords != grid_col]
  .x <- tidyr::expand_grid(grid,
                           vctrs::vec_unique(.x[coords])) |>
    dplyr::left_join(.x,
                     by = c(grid_col, coords))
  .x <- .x[names(.x) != grid_col]

  .x <- stars::st_as_stars(.x,
                           coords = c("X", "Y", coords),
                           y_decreasing = FALSE, ...) |>
    sf::st_set_crs(crs)
  dim_x <- dim(.x)
  .x |>
    dplyr::slice("X", 1L:(dim_x[["X"]] - 1L),
                 drop = FALSE) |>
    dplyr::slice("Y", 1L:(dim_x[["Y"]] - 1L),
                 drop = FALSE)
}

#' @export
plot.tbl_grid <- function(x, y,
                          as_points = FALSE, ...) {
  if (!missing(y)) {
    warn("`y` is ignored")
  }

  plot(x[[grid_column(x)]],
       as_points = as_points, ...)
}

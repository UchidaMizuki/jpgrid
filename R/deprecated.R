#' Grid square code vector
#'
#' `r lifecycle::badge("deprecated")`
#'
#' A series of functions return `grid` class for each grid size.
#' `grid_auto` returns automatically determine grid size by the largest
#' grid size.
#'
#' @name grid_class
#'
#' @param x A list or vector.
#' @param strict A logical scalar. Should the number of digits in the grid
#' square code match a given number of digits?
#'
#' @return A `grid` vector.
#'
#' @examples
#' grid_80km("53394526313")
#' grid_80km("53394526313", strict = FALSE)
#'
#' grid_auto(c("53394526313", "5339358633", "533945764"))
#' grid_auto(c("53394526313", "5339358633", "533945764"), strict = FALSE)
NULL

grid_impl <- function(x, strict, size, what) {
  lifecycle::deprecate_warn("0.4.0", stringr::str_c(what, "()"),
                            details = "Please use `grid_parse()` or `grid_convert()`")

  if (is_grid(x)) {
    grid_convert(x,
                 size = size)
  } else {
    grid_parse(x,
               size = size,
               strict = strict)
  }
}

#' @export
#' @rdname grid_class
grid_80km <- function(x,
                      strict = TRUE) {
  grid_impl(x,
            strict = strict,
            size = 80000L,
            what = "grid_80km")
}

#' @export
#' @rdname grid_class
grid_10km <- function(x,
                      strict = TRUE) {
  grid_impl(x,
            strict = strict,
            size = 10000L,
            what = "grid_10km")
}

#' @export
#' @rdname grid_class
grid_1km <- function(x,
                     strict = TRUE) {
  grid_impl(x,
            strict = strict,
            size = 1000L,
            what = "grid_1km")
}

#' @export
#' @rdname grid_class
grid_500m <- function(x,
                      strict = TRUE) {
  grid_impl(x,
            strict = strict,
            size = 500L,
            what = "grid_500m")
}

#' @export
#' @rdname grid_class
grid_250m <- function(x,
                      strict = TRUE) {
  grid_impl(x,
            strict = strict,
            size = 250L,
            what = "grid_250m")
}

#' @export
#' @rdname grid_class
grid_125m <- function(x,
                      strict = TRUE) {
  grid_impl(x,
            strict = strict,
            size = 125L,
            what = "grid_125m")
}

#' @export
#' @rdname grid_class
grid_100m <- function(x,
                      strict = TRUE) {
  grid_impl(x,
            strict = strict,
            size = 100L,
            what = "grid_100m")
}

#' @export
#' @rdname grid_class
grid_auto <- function(x,
                      strict = TRUE) {
  grid_impl(x,
            strict = strict,
            size = NULL,
            what = "grid_auto")
}

#' Converting sfc geometries to grid square codes
#'
#' `r lifecycle::badge("deprecated")`
#'
#' @param geometry A `sfc` vector.
#' @param size A grid size.
#' @param options Options vector for GDALRasterize passed on to
#' [stars::st_rasterize()].
#' @param ... Passed on to [stars::st_rasterize()].
#'
#' @return A list of `grid` vectors.
#'
#' @export
geometry_to_grid <- function(geometry, size,
                             options = "ALL_TOUCHED=TRUE", ...) {
  lifecycle::deprecate_warn("0.4.0", "geometry_to_grid()", "grid_from_geometry()")

  grid_from_geometry(geometry = geometry,
                     size = size,
                     options = options, ...)
}

#' Converting bbox to grid square codes
#'
#' `r lifecycle::badge("deprecated")`
#'
#' @param bbox A `bbox`.
#' @param size A grid size.
#'
#' @return A `grid` vector.
#'
#' @export
bbox_to_grid <- function(bbox, size) {
  lifecycle::deprecate_warn("0.4.0", "bbox_to_grid()", "grid_from_bbox()")

  grid_from_bbox(bbox = bbox,
                 size = size)
}

#' Convert a data frame into a tbl_grid object
#'
#' `r lifecycle::badge("deprecated")`
#'
#' The `tbl_grid` object is a data frame with `grid` objects in the columns.
#' `as_tbl_grid` converts a data frame into a tbl_grid object.
#'
#' @param x An object to be converted into an object class `tbl_grid`.
#' @param var A variable to specify the grid object. By default, the first
#' column of the grid object is taken.
#' @param size 	A grid size.
#' @param strict A logical scalar. Should the number of digits in the grid
#' square code match a given number of digits?
#' @param ... Additional arguments passed to [stickyr::new_sticky_tibble()]
#'
#' @return A `tbl_grid` object.
#'
#' @export
as_tbl_grid <- function(x,
                        var = NULL,
                        size = NULL,
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
                                purrr::partial(grid_parse,
                                               strict = strict,
                                               size = size)))

  tibble::new_tibble(out,
                     grid_col = var,
                     class = "tbl_grid")
}

grid_column <- function(x) {
  attr(x, "grid_col")
}

#' Conversion between grid square codes and coordinates (longitude and latitude)
#'
#' `r lifecycle::badge("deprecated")`
#'
#' @param X A numeric vector of longitude.
#' @param Y A numeric vector of latitude.
#' @param size A grid size.
#'
#' @return `XY_to_grid` returns a `grid` vector.
#'
#' @export
XY_to_grid <- function(X, Y, size) {
  lifecycle::deprecate_warn("0.4.0", "XY_to_grid()", "grid_from_XY()")
  grid_from_XY(X = X,
               Y = Y,
               size = size)
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

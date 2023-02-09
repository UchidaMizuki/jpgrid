#' Converting sfc geometries to grid square codes
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
grid_from_geom <- function(geometry, size,
                           options = "ALL_TOUCHED=TRUE", ...) {
  if (!inherits(geometry, "sfc")) {
    geometry <- sf::st_as_sfc(geometry)
  }

  if (inherits(geometry, "sfc_POINT")) {
    coords <- geometry |>
      sf::st_coordinates() |>
      tibble::as_tibble()

    grid_from_coords(X = coords$X,
                     Y = coords$Y,
                     size = size)
  } else {
    geometry |>
      purrr::map(function(x) {
        grid <- x |>
          sf::st_bbox() |>
          grid_from_bbox(size = size) |>
          st_as_stars()

        coords <- x |>
          sf::st_sfc() |>
          sf::st_as_sf() |>
          stars::st_rasterize(grid,
                              options = options, ...) |>
          sf::st_as_sf(as_points = TRUE) |>
          sf::st_coordinates() |>
          tibble::as_tibble()

        grid_from_coords(X = coords$X,
                         Y = coords$Y,
                         size = size)
      })
  }
}

#' Converting bbox to grid square codes
#'
#' @param bbox A `bbox`.
#' @param size A grid size.
#'
#' @return A `grid` vector.
#'
#' @export
grid_from_bbox <- function(bbox, size) {
  bbox <- sf::st_bbox(bbox)
  size <- grid_size_match(size)

  grid_min <- grid_from_coords(X = bbox[["xmin"]],
                               Y = bbox[["ymin"]],
                               size = size)
  n_X_min <- field(grid_min, "n_X")
  n_Y_min <- field(grid_min, "n_Y")

  grid_max <- grid_from_coords(X = bbox[["xmax"]],
                               Y = bbox[["ymax"]],
                               size = size)
  n_X_max <- field(grid_max, "n_X")
  n_Y_max <- field(grid_max, "n_Y")

  n_XY <- tidyr::expand_grid(n_X = n_X_min:n_X_max,
                             n_Y = n_Y_min:n_Y_max)

  new_grid(size = size,
           n_X = n_XY$n_X,
           n_Y = n_XY$n_Y)
}

#' @importFrom sf st_bbox
#' @export
st_bbox.grid <- function(obj, ...) {
  coords <- obj |>
    grid_to_coords(center = FALSE)
  st_bbox(c(xmin = min(coords$X_min),
            ymin = min(coords$Y_min),
            xmax = max(coords$X_max),
            ymax = max(coords$Y_max)), ...)
}

#' @importFrom sf st_as_sfc
#' @export
st_as_sfc.grid <- function(x,
                           as_points = FALSE,
                           crs = sf::NA_crs_, ...) {
  geometry <- tibble::tibble(grid = x) |>
    vec_unique()
  geometry <- vec_slice(geometry ,
                        !is.na(geometry$grid))

  if (!as_points) {
    coords <- grid_to_coords(geometry$grid,
                             center = FALSE)
    geometry$geometry <- list(coords$X_min, coords$Y_min, coords$X_max, coords$Y_max) |>
      purrr::pmap(function(X_min, Y_min, X_max, Y_max) {
        if (is.na(X_min) || is.na(Y_min) || is.na(X_max) || is.na(Y_max)) {
          sf::st_polygon() |>
            sf::st_sfc(...)
        } else {
          sf::st_bbox(c(xmin = X_min,
                        ymin = Y_min,
                        xmax = X_max,
                        ymax = Y_max)) |>
            sf::st_as_sfc(...)
        }
      }) |>
      purrr::list_c()
  } else {
    geometry$geometry <- grid_to_coords(geometry$grid,
                                        center = TRUE) |>
      sf::st_as_sf(coords = c("X", "Y"), ...) |>
      sf::st_geometry()
  }

  tibble::tibble(grid = x) |>
    dplyr::left_join(geometry,
                     by = "grid") |>
    purrr::chuck("geometry") |>
    sf::st_set_crs(crs)
}

#' @export
plot.grid <- function(x, y,
                      as_points = FALSE, ...) {
  if (!missing(y)) {
    warn("`y` is ignored")
  }

  x |>
    st_as_sfc(as_points = as_points) |>
    plot(...)
}

#' Converting data frame containing grid square codes to sf
#'
#' @param x A data frame or a `grid`.
#' @param as_points Return the center points of the grids or not?
#' @param crs Coordinate reference system.
#' @param grid_column_name A scalar character.
#' @param ... passed on to [sf::st_as_sf()].
#'
#' @return A \code{sf} object.
#'
#' @export
grid_as_sf <- function(x,
                       as_points = FALSE,
                       crs = sf::NA_crs_,
                       grid_column_name = NULL, ...) {
  if (is_grid(x)) {
    x <- tibble::tibble(grid = x)
    grid_column_name <- "grid"
  } else if (!is.data.frame(x)) {
    cli_abort("{.arg x} must be a {.cls grid} or a data frame.")
  }

  if (is.null(grid_column_name)) {
    i <- x |>
      purrr::map_lgl(is_grid)
    grid_column_name <- names(x) |>
      vec_slice(i) |>
      vec_slice(1L)
  }
  grid <- x[[grid_column_name]]

  x |>
    sf::st_set_geometry(grid |>
                          st_as_sfc(as_points = as_points,
                                    crs = crs)) |>
    sf::st_as_sf(...)
}

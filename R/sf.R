#' Converting sfc geometries to grid square codes
#'
#' @param geometry A `sfc` vector.
#' @param grid_size A grid size.
#' @param options Options vector for GDALRasterize passed on to
#' [stars::st_rasterize()].
#' @param ... Passed on to [stars::st_rasterize()].
#'
#' @return A list of `grid` vectors.
#'
#' @export
geometry_to_grid <- function(geometry, grid_size,
                             options = "ALL_TOUCHED=TRUE", ...) {
  if (!inherits(geometry, "sfc")) {
    geometry <- sf::st_as_sfc(geometry)
  }

  if (inherits(geometry, "sfc_POINT")) {
    coords <- geometry |>
      sf::st_coordinates() |>
      tibble::as_tibble()

    coords_to_grid(X = coords$X,
                   Y = coords$Y,
                   grid_size = grid_size)
  } else {
    geometry <- geometry |>
      sf::st_as_sf() |>
      tibble::rowid_to_column("id")

    grid <- geometry |>
      sf::st_bbox() |>
      bbox_to_grid(grid_size = grid_size) |>
      st_as_stars()

    geometry_grid <- geometry |>
      stars::st_rasterize(grid,
                          options = options, ...) |>
      sf::st_as_sf() |>
      sf::st_set_crs(sf::st_crs(geometry)) |>
      dplyr::select() |>
      sf::st_join(geometry) |>
      dplyr::mutate(.data$geometry |>
                      sf::st_centroid() |>
                      sf::st_coordinates() |>
                      tibble::as_tibble()) |>
      sf::st_drop_geometry() |>
      dplyr::mutate(grid = coords_to_grid(X = .data$X,
                                          Y = .data$Y,
                                          grid_size = grid_size)) |>
      dplyr::distinct(.data$id, .data$grid) |>
      dplyr::summarise(dplyr::across("grid", list),
                       .by = "id")

    geometry |>
      sf::st_drop_geometry() |>
      dplyr::left_join(geometry_grid,
                       by = "id") |>
      dplyr::pull("grid")
  }
}

#' Converting bbox to grid square codes
#'
#' @param bbox A `bbox`.
#' @param grid_size A grid size.
#'
#' @return A `grid` vector.
#'
#' @export
bbox_to_grid <- function(bbox, grid_size) {
  bbox <- sf::st_bbox(bbox)
  grid_size <- grid_size_match(grid_size)

  grid_min <- coords_to_grid(X = bbox[["xmin"]],
                             Y = bbox[["ymin"]],
                             grid_size = grid_size)
  n_X_min <- field(grid_min, "n_X")
  n_Y_min <- field(grid_min, "n_Y")

  grid_max <- coords_to_grid(X = bbox[["xmax"]],
                             Y = bbox[["ymax"]],
                             grid_size = grid_size)
  n_X_max <- field(grid_max, "n_X")
  n_Y_max <- field(grid_max, "n_Y")

  n_XY <- tidyr::expand_grid(n_X = n_X_min:n_X_max,
                             n_Y = n_Y_min:n_Y_max)

  new_grid(grid_size = grid_size,
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
  x <- tibble::tibble(grid = x)

  x_unique <- vec_unique(x)
  x_unique <- vec_slice(x_unique,
                        !is.na(x_unique$grid))

  if (as_points) {
    x_unique$geometry <- grid_to_coords(x_unique$grid,
                                        center = TRUE) |>
      sf::st_as_sf(coords = c("X", "Y"), ...) |>
      sf::st_geometry()
  } else {
    geometry <- st_as_sfc(x_unique$grid,
                          as_points = TRUE) |>
      sf::st_as_sf() |>
      stars::st_rasterize(st_as_stars(x_unique$grid)) |>
      sf::st_as_sf(...)
    x_unique$geometry <- vec_slice(sf::st_geometry(geometry),
                          vec_match(vec_seq_along(x_unique), geometry$ID))
  }

  x |>
    dplyr::left_join(x_unique,
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
#' @return A `sf` object.
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

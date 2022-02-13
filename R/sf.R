#' Converting sfc points to regional grids
#'
#' @param point A \code{sfc_POINT} vector.
#' @inheritParams size
#'
#' @return A \code{grid} class vector.
#'
#' @export
point_to_grid <- function(point, size) {
  stopifnot(inherits(point, "sfc_POINT"))

  point <- point %>%
    sf::st_coordinates() %>%
    tibble::as_tibble()

  XY_to_grid(X = point$X,
             Y = point$Y,
             size = size)
}

#' Converting sfc geometries to regional grids
#'
#' @param geometry A \code{sfc} vector.
#' @inheritParams size
#' @param ... Passed on to \code{stars::st_rasterize()}.
#'
#' @return A list of \code{grid} class vectors.
#'
#' @export
geometry_to_grid <- function(geometry, size, ...) {
  if (!inherits(geometry, "sfc")) {
    geometry <- sf::st_as_sfc(geometry)
  }

  geometry %>%
    purrr::map(function(x) {
      grid <- x %>%
        sf::st_bbox() %>%
        bbox_to_grid(size = size) %>%
        grid_as_stars()

      XY <- x %>%
        sf::st_sfc() %>%
        sf::st_as_sf() %>%
        stars::st_rasterize(grid, ...) %>%
        sf::st_as_sf(as_points = TRUE) %>%
        sf::st_coordinates() %>%
        tibble::as_tibble()

      XY_to_grid(X = XY$X,
                 Y = XY$Y,
                 size = size)
    })
}

#' Converting bbox to regional grids
#'
#' @param bbox A \code{bbox} or a list of \code{bbox}.
#' @inheritParams size
#'
#' @return A \code{grid} vector (when bbox is a \code{bbox}) or A list of \code{grid} vectors (when bbox is a list of \code{bbox}).
#'
#' @export
bbox_to_grid <- function(bbox, size) {
  if (is.list(bbox)) {
    bbox %>%
      purrr::map(function(bbox) {
        bbox_to_grid(bbox, size)
      })
  } else {
    bbox <- sf::st_bbox(bbox)
    size <- size_match(size)

    grid_min <- XY_to_grid(X = bbox[["xmin"]],
                           Y = bbox[["ymin"]],
                           size = size)
    n_X_min <- field(grid_min, "n_X")
    n_Y_min <- field(grid_min, "n_Y")

    grid_max <- XY_to_grid(X = bbox[["xmax"]],
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
}

#' Converting regional grids to sfc geometries
#'
#' @param x A \code{grid} vector.
#' @param as_points Return the center points of the grids or not?
#' @param crs Coordinate reference system.
#'
#' @return A \code{sfc_POLYGON} vector (\code{as_points = FALSE}) or a \code{sfc_POINT} vector ((\code{as_points = TRUE})).
#'
#' @export
grid_as_sfc <- function(x,
                        as_points = FALSE,
                        crs = sf::NA_crs_) {
  stopifnot(is_grid(x))

  geometry <- tibble::tibble(grid = x) %>%
    vec_unique()
  geometry <- vec_slice(geometry ,
                        !is.na(geometry$grid))

  if (!as_points) {
    XY <- grid_to_XY(geometry$grid,
                     center = FALSE)
    geometry$geometry <- list(XY$X_min, XY$Y_min, XY$X_max, XY$Y_max) %>%
      purrr::pmap(function(X_min, Y_min, X_max, Y_max) {
        if (is.na(X_min) || is.na(Y_min) || is.na(X_max) || is.na(Y_max)) {
          sf::st_polygon() %>%
            sf::st_sfc()
        } else {
          sf::st_bbox(c(xmin = X_min,
                        ymin = Y_min,
                        xmax = X_max,
                        ymax = Y_max)) %>%
            sf::st_as_sfc()
        }
      }) %>%
      purrr::reduce(c)
  } else {
    geometry$geometry <- grid_to_XY(geometry$grid,
                                    center = TRUE) %>%
      sf::st_as_sf(coords = c("X", "Y")) %>%
      sf::st_geometry()
  }

  tibble::tibble(grid = x) %>%
    dplyr::left_join(geometry,
                     by = "grid") %>%
    purrr::chuck("geometry") %>%
    sf::st_set_crs(crs)
}

#' Converting data frame containing regional grids to sf
#'
#' @param x A data frame.
#' @param as_points Return the center points of the grids or not?
#' @param crs Coordinate reference system.
#' @param grid_column_name A scalar character.
#' @param ... passed on to \code{sf::st_as_sf()}.
#'
#' @return A \code{sf} object.
#'
#' @export
grid_as_sf <- function(x,
                       as_points = FALSE,
                       crs = sf::NA_crs_,
                       grid_column_name = NULL,
                       ...) {
  if (is_grid(x)) {
    x <- tibble::tibble(grid = x)
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

  x %>%
    sf::st_set_geometry(grid %>%
                          grid_as_sfc(as_points = as_points,
                                      crs = crs)) %>%
    sf::st_as_sf(...)
}

#' @export
plot.grid <- function(x, y,
                      as_points = FALSE,
                      ...) {
  stopifnot(missing(y))

  x %>%
    grid_as_sfc(as_points = as_points) %>%
    plot(...)
}

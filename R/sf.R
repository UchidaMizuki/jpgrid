#' Converting sfc geometries to grid square codes
#'
#' @param geometry A `sfc` vector.
#' @inheritParams size
#' @param options Options vector for GDALRasterize passed on to
#' [stars::st_rasterize()].
#' @param ... Passed on to [stars::st_rasterize()].
#'
#' @return A list of `grid` vectors.
#'
#' @export
geometry_to_grid <- function(geometry, size,
                             options = "ALL_TOUCHED=TRUE", ...) {
  if (!inherits(geometry, "sfc")) {
    geometry <- sf::st_as_sfc(geometry)
  }

  if (inherits(geometry, "sfc_POINT")) {
    XY <- geometry %>%
      sf::st_coordinates() %>%
      tibble::as_tibble()

    XY_to_grid(X = XY$X,
               Y = XY$Y,
               size = size)
  } else {
    geometry %>%
      purrr::map(function(x) {
        grid <- x %>%
          sf::st_bbox() %>%
          bbox_to_grid(size = size) %>%
          st_as_stars()

        XY <- x %>%
          sf::st_sfc() %>%
          sf::st_as_sf() %>%
          stars::st_rasterize(grid,
                              options = options, ...) %>%
          sf::st_as_sf(as_points = TRUE) %>%
          sf::st_coordinates() %>%
          tibble::as_tibble()

        XY_to_grid(X = XY$X,
                   Y = XY$Y,
                   size = size)
      })
  }
}

#' Converting bbox to grid square codes
#'
#' @param bbox A `bbox`.
#' @inheritParams size
#'
#' @return A `grid` vector.
#'
#' @export
bbox_to_grid <- function(bbox, size) {
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

#' @importFrom sf st_bbox
#' @export
st_bbox.grid <- function(obj, ...) {
  XY <- obj %>%
    grid_to_XY(center = FALSE)
  st_bbox(c(xmin = min(XY$X_min),
            ymin = min(XY$Y_min),
            xmax = max(XY$X_max),
            ymax = max(XY$Y_max)), ...)
}

#' @export
st_bbox.tbl_grid <- function(obj, ...) {
  obj <- obj[[grid_column(obj)]]
  st_bbox(obj)
}

#' @importFrom sf st_as_sfc
#' @export
st_as_sfc.grid <- function(x,
                           as_points = FALSE,
                           crs = sf::NA_crs_, ...) {
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
            sf::st_sfc(...)
        } else {
          sf::st_bbox(c(xmin = X_min,
                        ymin = Y_min,
                        xmax = X_max,
                        ymax = Y_max)) %>%
            sf::st_as_sfc(...)
        }
      }) %>%
      purrr::reduce(c)
  } else {
    geometry$geometry <- grid_to_XY(geometry$grid,
                                    center = TRUE) %>%
      sf::st_as_sf(coords = c("X", "Y"), ...) %>%
      sf::st_geometry()
  }

  tibble::tibble(grid = x) %>%
    dplyr::left_join(geometry,
                     by = "grid") %>%
    purrr::chuck("geometry") %>%
    sf::st_set_crs(crs)
}

#' @importFrom sf st_as_sf
#' @export
st_as_sf.tbl_grid <- function(x,
                              as_points = FALSE,
                              crs = sf::NA_crs_, ...) {
  grid <- x[[grid_column(x)]]

  x %>%
    tibble::as_tibble() %>%
    sf::st_set_geometry(grid %>%
                          st_as_sfc(as_points = as_points,
                                    crs = crs)) %>%
    sf::st_as_sf(...)
}

#' @export
plot.grid <- function(x, y,
                      as_points = FALSE, ...) {
  if (!missing(y)) {
    warn("`y` is ignored")
  }

  x %>%
    st_as_sfc(as_points = as_points) %>%
    plot(...)
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

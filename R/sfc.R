#' @export
point_to_mesh <- function(point, size) {
  stopifnot(inherits(point, "sfc_POINT"))

  point <- point %>%
    sf::st_coordinates() %>%
    tibble::as_tibble()

  XY_to_mesh(X = point$X,
             Y = point$Y,
             size = size)
}

# FIXME?
mesh_to_sfc <- function(mesh,
                        type = "POLYGON",
                        crs = sf::NA_crs_) {
  stopifnot(is_mesh(mesh))
  arg_match(type, c("POLYGON", "POINT"))

  geom <- tibble::tibble(mesh = mesh) %>%
    vec_unique() %>%
    dplyr::filter(!is.na(mesh))

  if (type == "POLYGON") {
    geom <- geom %>%
      dplyr::mutate(mesh_to_XY(mesh,
                               center = F)) %>%
      dplyr::mutate(geom = list(X_min, Y_min, X_max, Y_max) %>%
                      purrr::pmap(function(X_min, Y_min, X_max, Y_max) {
                        if (is.na(X_min) || is.na(Y_min) || is.na(X_max) || is.na(Y_max)) {
                          sf::st_polygon() %>%
                            sf::st_sfc(crs = crs)
                        } else {
                          sf::st_bbox(c(xmin = X_min,
                                        ymin = Y_min,
                                        xmax = X_max,
                                        ymax = Y_max)) %>%
                            sf::st_as_sfc(crs = crs)
                        }
                      })) %>%
      dplyr::select(mesh, geom) %>%
      tidyr::unnest(geom)
  } else {
    geom <- geom %>%
      dplyr::mutate(mesh_to_XY(mesh)) %>%
      sf::st_as_sf(coords = c("X", "Y"),
                   crs = crs)
  }

  tibble::tibble(mesh = mesh) %>%
    dplyr::left_join(geom,
                     by = "mesh") %>%
    sf::st_as_sf() %>%
    sf::st_geometry()
}

#' @export
mesh_to_polygon <- function(mesh,
                            crs = sf::NA_crs_) {
  mesh %>%
    mesh_to_sfc(type = "POLYGON",
                crs = crs)
}

#' @export
mesh_to_point <- function(mesh,
                          crs = sf::NA_crs_) {
  mesh %>%
    mesh_to_sfc(type = "POINT",
                crs = crs)
}

#' @export
plot.mesh <- function(x, y,
                      type = "POLYGON",
                      ...) {
  stopifnot(missing(y))

  x %>%
    mesh_to_sfc(type = type) %>%
    plot(...)
}

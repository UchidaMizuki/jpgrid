# FIXME?
mesh_to_sfc <- function(mesh,
                        type = "POLYGON",
                        crs = sf::NA_crs_) {
  stopifnot(is_mesh(mesh))
  arg_match(type, c("POLYGON", "POINT"))

  mesh_unique <- vec_unique(mesh)
  mesh_unique <- mesh_unique %>%
    vec_slice(!is.na(mesh_unique))

  if (vec_is_empty(mesh_unique)) {
    if (type == "POLYGON") {
      geom <- sf::st_polygon()
    } else {
      geom <- sf::st_point()
    }

    geom %>%
      sf::st_sfc(crs = crs) %>%
      vec_rep(vec_size(mesh))
  } else {
    if (type == "POLYGON") {
      geom <- tibble::tibble(mesh = mesh_unique) %>%
        dplyr::mutate(mesh_to_XY(mesh,
                                 center = F)) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(geom = {
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
        },
        .keep = "unused") %>%
        dplyr::ungroup()
    } else {
      geom <- tibble::tibble(mesh = mesh_unique) %>%
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

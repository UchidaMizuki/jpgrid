# FIXME?
mesh_to_sfc <- function(mesh,
                        type = "POLYGON",
                        crs = sf::NA_crs_) {
  stopifnot(is_mesh(mesh))
  arg_match(type, c("POLYGON", "POINT"))

  geom <- tibble::tibble(mesh = vec_unique(mesh)) %>%
    dplyr::mutate(mesh_to_XY(mesh,
                             center = F)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(geom = {
      if (type == "POLYGON") {
        if (is_na(X_min) || is_na(Y_min) || is_na(X_max) || is_na(Y_max)) {
          sf::st_polygon() %>%
            sf::st_sfc(crs = crs)
        } else {
          sf::st_bbox(c(xmin = X_min,
                        ymin = Y_min,
                        xmax = X_max,
                        ymax = Y_max)) %>%
            sf::st_as_sfc(crs = crs)
        }
      } else {
        sf::st_point(c(X, Y)) %>%
          sf::st_sfc(crs = crs)
      }
    },
    .keep = "unused") %>%
    dplyr::ungroup()

  tibble::tibble(mesh = mesh) %>%
    dplyr::left_join(geom,
                     by = "mesh") %>%
    dplyr::pull(geom)
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

# FIXME?
#' @export
mesh_to_sfc <- function(mesh, crs = sf::NA_crs_, ...) {
  stopifnot(is_mesh(mesh))

  bbox <- tibble::tibble(mesh = vec_unique(mesh)) %>%
    dplyr::mutate(mesh_to_XY(mesh,
                             center = F)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(bbox = {
      if (is_na(X_min) || is_na(Y_min) || is_na(X_max) || is_na(Y_max)) {
        sf::st_polygon() %>%
          sf::st_sfc(crs = crs) %>%
          list()
      } else {
        sf::st_bbox(c(xmin = X_min,
                      ymin = Y_min,
                      xmax = X_max,
                      ymax = Y_max)) %>%
          sf::st_as_sfc(crs = crs) %>%
          list()
      }
    },
    .keep = "unused") %>%
    dplyr::ungroup() %>%
    tidyr::unnest(bbox)

  tibble::tibble(mesh = mesh) %>%
    dplyr::left_join(bbox,
                     by = "mesh") %>%
    dplyr::pull(bbox)
}

#' @export
plot.mesh <- function(x, y, ...) {
  x %>%
    mesh_to_sfc() %>%
    plot()
}

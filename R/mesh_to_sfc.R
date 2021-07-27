#' @export
mesh_to_sfc <- function(mesh, crs = sf::NA_crs_, ...) {
  UseMethod("mesh_to_sfc")
}
#' @export
mesh_to_sfc.default <- function(mesh, crs = sf::NA_crs_, ...) {
  mesh <- as_mesh(mesh, ...)
  mesh_to_sfc(mesh, crs)
}
#' @export
mesh_to_sfc.mesh_80km <- function(mesh, crs = sf::NA_crs_, ...) {
  mesh %>%
    mesh_to_XY(center = F) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(bbox = sf::st_bbox(c(xmin = X_min,
                                       ymin = Y_min,
                                       xmax = X_max,
                                       ymax = Y_max)) %>%
                    sf::st_as_sfc() %>%
                    sf::st_set_crs(crs),
                  .keep = "unused") %>%
    dplyr::pull(bbox)
}

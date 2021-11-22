#' Converting sfc points to regional meshes
#'
#' @param point A \code{sfc_POINT} vector.
#' @inheritParams size
#'
#' @return A \code{mesh} class vector.
#'
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

#' Converting sfc polygons to regional meshes
#'
#' @param sfc A \code{sfc} vector.
#' @inheritParams size
#' @param .predicate A \code{.predicate} parameter for \code{sf::st_filter} function.
#'
#' @return A list of \code{mesh} class vectors.
#'
#' @export
sfc_to_mesh <- function(sfc, size,
                        .predicate = sf::st_intersects) {
  stopifnot(inherits(sfc, "sfc"))

  mesh <- sfc %>%
    purrr::map(sf::st_bbox) %>%
    bbox_to_mesh(size = size) %>%
    purrr::modify(function(mesh) {
      tibble::tibble(mesh = mesh,
                     polygon_mesh = mesh_to_polygon(mesh)) %>%
        sf::st_as_sf()
    })

  purrr::map2(mesh, sfc,
              function(mesh, sfc) {
                mesh %>%
                  sf::st_filter(sfc,
                                .predicate = .predicate) %>%
                  purrr::pluck("mesh")
              })
}

#' Converting bbox to regional meshes
#'
#' @param bbox A \code{bbox} or a list of \code{bbox}.
#' @inheritParams size
#'
#' @return A \code{mesh} vector (when bbox is a \code{bbox}) or A list of \code{mesh} vectors (when bbox is a list of \code{bbox}).
#'
#' @export
bbox_to_mesh <- function(bbox, size) {
  if (inherits(bbox, "bbox")) {
    mesh_grid(X_min = bbox[["xmin"]],
              Y_min = bbox[["ymin"]],
              X_max = bbox[["xmax"]],
              Y_max = bbox[["ymax"]],
              size = size) %>%
      dplyr::first()
  } else {
    stopifnot(is.list(bbox))

    bbox <- bbox %>%
      purrr::map_dfr(function(bbox) {
        stopifnot(inherits(bbox, "bbox"))

        tibble::tibble(X_min = bbox[["xmin"]],
                       Y_min = bbox[["ymin"]],
                       X_max = bbox[["xmax"]],
                       Y_max = bbox[["ymax"]])
      })

    mesh_grid(X_min = bbox$X_min,
              Y_min = bbox$Y_min,
              X_max = bbox$X_max,
              Y_max = bbox$Y_max,
              size = size)
  }
}

#' Converting regional meshes to sfc geometries
#'
#' @name sfc
#'
#' @inheritParams mesh
#' @param crs Coordinate reference system.
#'
#' @return \code{mesh_to_polygon} returns a \code{sfc_POLYGON} vector.
#' \code{mesh_to_point} returns a \code{sfc_POINT} vector.
NULL

mesh_to_sfc <- function(mesh,
                        type = "POLYGON",
                        crs = sf::NA_crs_) {
  stopifnot(is_mesh(mesh))
  arg_match(type, c("POLYGON", "POINT"))

  geom <- tibble::tibble(mesh = mesh) %>%
    vec_unique()
  geom <- vec_slice(geom,
                    !is.na(geom$mesh))

  if (type == "POLYGON") {
    XY <- mesh_to_XY(geom$mesh,
                     center = FALSE)
    geom$geom <- list(XY$X_min, XY$Y_min, XY$X_max, XY$Y_max) %>%
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
      }) %>%
      purrr::reduce(c)
  } else {
    geom$geom <- mesh_to_XY(geom$mesh,
                            center = TRUE) %>%
      sf::st_as_sf(coords = c("X", "Y"),
                   crs = crs) %>%
      sf::st_geometry()
  }

  tibble::tibble(mesh = mesh) %>%
    dplyr::left_join(geom,
                     by = "mesh") %>%
    sf::st_as_sf() %>%
    sf::st_geometry()
}

#' @export
#'
#' @rdname sfc
mesh_to_polygon <- function(mesh,
                            crs = sf::NA_crs_) {
  mesh %>%
    mesh_to_sfc(type = "POLYGON",
                crs = crs)
}

#' @export
#' @rdname sfc
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

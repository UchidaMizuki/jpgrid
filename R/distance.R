# FIXME?
#' @export
mesh_distance <- function(mesh, mesh_to,
                          close = F,
                          type = "keep_na") {
  if (is_mesh(mesh)) {
    stopifnot(is_mesh(mesh_to))

    size <- mesh_size(mesh)
    stopifnot(size == mesh_size(mesh_to))

    mesh <- tibble::tibble(mesh = mesh,
                           mesh_to = mesh_to)

    distance <- mesh %>%
      vec_unique() %>%
      dplyr::filter(!is.na(mesh),
                    !is.na(mesh_to)) %>%
      dplyr::mutate(mesh_to_XY(mesh),
                    mesh_to_XY(mesh_to) %>%
                      dplyr::rename(X_to = X,
                                    Y_to = Y)) %>%

      dplyr::mutate(distance = geosphere::distGeo(p1 = cbind(X, Y),
                                                  p2 = cbind(X_to, Y_to)) %>%
                      units::set_units(m)) %>%
      dplyr::select(mesh, mesh_to, distance)

    mesh %>%
      dplyr::left_join(distance,
                       by = c("mesh", "mesh_to")) %>%
      purrr::chuck("distance")
  } else {
    stopifnot(is_list(mesh),
              missing(mesh_to))
    arg_match(type, c("keep_na", "ignore_na", "skip_na"))

    mesh %>%
      purrr::modify(function(mesh) {
        if (type == "skip_na") {
          mesh <- mesh %>%
            vec_slice(!is.na(mesh))
        }

        if (close) {
          mesh_to <- c(tail(mesh, -1), mesh[1])
        } else {
          mesh_to <- tail(mesh, -1)
          mesh <- head(mesh, -1)
        }

        mesh_distance(mesh, mesh_to) %>%
          sum(na.rm = type == "ignore_na")
      })
  }
}


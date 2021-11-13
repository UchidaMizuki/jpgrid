#' @export
mesh_move <- function(mesh, n_X, n_Y, ...) {
  stopifnot(is_mesh(mesh))

  mesh(n_X = field(mesh, "n_X") + n_X,
       n_Y = field(mesh, "n_Y") + n_Y,
       size = mesh_size(mesh))
}

# FIXME?
#' @export
mesh_neighbor <- function(mesh, moore = T, simplify = T, ...) {
  n_XY <- tidyr::expand_grid(n_X = -1:1,
                             n_Y = -1:1) %>%
    dplyr::filter(n_X != 0 | n_Y != 0,
                  # Moore neighborhood or Von Neumann neighborhood
                  moore | n_X * n_Y == 0)

  neighbor <- tibble::tibble(mesh = vec_unique(mesh)) %>%
    tidyr::expand_grid(n_XY) %>%
    dplyr::mutate(mesh_neighbor = mesh %>%
                    mesh_move(n_X, n_Y)) %>%
    dplyr::group_nest(mesh,
                      .key = "neighbor")

  if (simplify) {
    neighbor <- neighbor %>%
      dplyr::rowwise() %>%
      dplyr::mutate(neighbor = neighbor %>%
                      purrr::chuck("mesh_neighbor") %>%
                      list()) %>%
      dplyr::ungroup()
  }

  tibble::tibble(mesh = mesh) %>%
    dplyr::left_join(neighbor,
                     by = "mesh") %>%
    purrr::chuck("neighbor")
}

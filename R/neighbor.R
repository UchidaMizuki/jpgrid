#' @export
mesh_neighbor <- function(mesh,
                          moore = T,
                          simplify = T) {
  UseMethod("mesh_neighbor")
}
#' @export
mesh_neighbor.default <- function(mesh,
                                  moore = T,
                                  simplify = T) {
  mesh <- as_mesh(mesh)
  mesh_neighbor(mesh, moore, simplify)
}
#' @export
mesh_neighbor.mesh_80km <- function(mesh,
                                    moore = T,
                                    simplify = T) {
  n_XY <- tidyr::expand_grid(n_X = -1:1,
                             n_Y = -1:1) %>%
    dplyr::filter(n_X != 0 | n_Y != 0,
                  moore | n_X * n_Y == 0)

  nbr <- tibble::tibble(mesh = vec_unique(mesh)) %>%
    tidyr::expand_grid(n_XY) %>%
    dplyr::mutate(mesh_neighbor = mesh_move(mesh, n_X, n_Y)) %>%
    dplyr::group_nest(mesh)

  if (simplify) {
    nbr <- nbr %>%
      dplyr::mutate(data = data %>%
                      purrr::map(~ .x %>%
                                   dplyr::pull(mesh_neighbor)))
  }

  tibble::tibble(mesh = mesh) %>%
    dplyr::left_join(nbr,
                     by = "mesh") %>%
    dplyr::pull(data)
}

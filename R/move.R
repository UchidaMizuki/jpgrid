#' @export
mesh_move <- function(mesh, n_X, n_Y, ...) {
  stopifnot(is_mesh(mesh))

  new_mesh(n_X = field(mesh, "n_X") + n_X,
           n_Y = field(mesh, "n_Y") + n_Y,
           size = mesh_size(mesh))
}

# FIXME?
#' @export
mesh_neighbor <- function(mesh,
                          n = 1L,
                          include_self = F,
                          moore = T,
                          simplify = T,
                          ...) {
  stopifnot(n > 0,
            n %% 1 == 0)

  n_XY <- tidyr::expand_grid(n_X = -n:n,
                             n_Y = -n:n) %>%
    dplyr::filter(include_self | n_X != 0 | n_Y != 0,
                  moore | abs(n_X) + abs(n_Y) <= n) # Moore neighborhood or Von Neumann neighborhood

  neighbor <- tibble::tibble(mesh = mesh) %>%
    vec_unique() %>%
    tidyr::expand_grid(n_XY) %>%
    dplyr::mutate(mesh_neighbor = mesh %>%
                    mesh_move(n_X, n_Y)) %>%
    dplyr::group_nest(mesh,
                      .key = "neighbor")

  if (simplify) {
    neighbor <- neighbor %>%
      dplyr::mutate(neighbor = neighbor %>%
                      purrr::map(function(neighbor) {
                        neighbor %>%
                          purrr::chuck("mesh_neighbor")
                      }))
  }

  tibble::tibble(mesh = mesh) %>%
    dplyr::left_join(neighbor,
                     by = "mesh") %>%
    purrr::chuck("neighbor")
}

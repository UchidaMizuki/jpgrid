#' Moving on the mesh
#'
#' @inheritParams mesh
#' @param n_X Number of moving cells in the longitude direction.
#' @param n_Y Number of moving cells in the latitude direction.
#'
#' @export
mesh_move <- function(mesh, n_X, n_Y) {
  stopifnot(is_mesh(mesh))

  new_mesh(size = mesh_size(mesh),
           n_X = field(mesh, "n_X") + n_X,
           n_Y = field(mesh, "n_Y") + n_Y)
}

#' Neighbor mesh
#'
#' @inheritParams mesh
#' @param n A numeric vector of degrees.
#' @param moore Moore neighborhood (\code{TRUE}) or Von Neumann neighborhood (\code{FALSE}).
#' @param simplify Should simplify the format of the return?
#'
#' @export
mesh_neighbor <- function(mesh,
                          n = 1L,
                          moore = T,
                          simplify = T) {
  stopifnot(n >= 0,
            n %% 1 == 0)

  n_XY <- n %>%
    purrr::map_dfr(function(n) {
      tidyr::expand_grid(n_X = -n:n,
                         n_Y = -n:n) %>%
        dplyr::filter(!moore | abs(n_X) == n | abs(n_Y) == n,
                      moore | (abs(n_X) + abs(n_Y)) == n)
    })

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

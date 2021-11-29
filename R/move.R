#' Moving on regional meshes
#'
#' @inheritParams mesh
#' @param n_X Number of moving cells in the longitude direction.
#' @param n_Y Number of moving cells in the latitude direction.
#'
#' @return A \code{mesh} class vector.
#'
#' @export
mesh_move <- function(mesh, n_X, n_Y) {
  stopifnot(is_mesh(mesh))

  new_mesh(size = mesh_size(mesh),
           n_X = field(mesh, "n_X") + n_X,
           n_Y = field(mesh, "n_Y") + n_Y)
}

#' Neighborhood regional mesh
#'
#' @inheritParams mesh
#' @param n A numeric vector of degrees.
#' @param moore Moore neighborhood (\code{TRUE}) or Von Neumann neighborhood (\code{FALSE}).
#' @param simplify Should simplify the format of the return?
#'
#' @return A list of \code{mesh} class vectors.
#'
#' @export
mesh_neighbor <- function(mesh,
                          n = 1L,
                          moore = TRUE,
                          simplify = TRUE) {
  stopifnot(n >= 0L,
            n %% 1L == 0L)

  n_XY <- n %>%
    purrr::map_dfr(function(n) {
      n_XY <- tidyr::expand_grid(n = n,
                                 n_X = -n:n,
                                 n_Y = -n:n)
      vec_slice(n_XY,
                (!moore | abs(n_XY$n_X) == n | abs(n_XY$n_Y) == n) &
                  (moore | (abs(n_XY$n_X) + abs(n_XY$n_Y)) == n))
    })

  neighbor <- tibble::tibble(mesh = mesh) %>%
    vec_unique() %>%
    tidyr::expand_grid(n_XY)
  neighbor$mesh_neighbor <- neighbor$mesh %>%
    mesh_move(n_X = neighbor$n_X,
              n_Y = neighbor$n_Y)
  neighbor <- neighbor %>%
    dplyr::group_nest(mesh,
                      .key = "neighbor")

  if (simplify) {
    neighbor$neighbor <- neighbor$neighbor %>%
      purrr::map(function(neighbor) {
        neighbor %>%
          purrr::chuck("mesh_neighbor")
      })
  }

  tibble::tibble(mesh = mesh) %>%
    dplyr::left_join(neighbor,
                     by = "mesh") %>%
    purrr::chuck("neighbor")
}

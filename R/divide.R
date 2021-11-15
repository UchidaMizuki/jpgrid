#' @export
mesh_divide <- function(mesh, size) {
  stopifnot(is_mesh(mesh))

  size <- size_match(size)
  ratio <- mesh_size(mesh) / size

  stopifnot(ratio %% 1 == 0)

  n_X <- field(mesh, "n_X")
  n_Y <- field(mesh, "n_Y")

  purrr::map2(n_X, n_Y,
              function(n_X, n_Y) {
                n_X_min <- n_X * ratio
                n_X_max <- (n_X + 1) * ratio - 1

                n_Y_min <- n_Y * ratio
                n_Y_max <- (n_Y + 1) * ratio - 1

                tidyr::expand_grid(n_X = n_X_min:n_X_max,
                                   n_Y = n_Y_min:n_Y_max) %>%
                  dplyr::mutate(mesh = new_mesh(n_X = n_X,
                                                n_Y = n_Y,
                                                size = size)) %>%
                  purrr::chuck("mesh")
              })
}

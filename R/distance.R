#' Distance between meshes
#'
#' If \code{mesh} and \code{mesh_to} are both vectors, the distance between \code{mesh} and \code{mesh_to} is calculated.
#' If \code{mesh} is a list, The path distance of each element is calculated.
#'
#' @inheritParams mesh_to
#' @param type How is the NA mesh treated when \code{mesh} is a list? \code{"skip_na"} skips the \code{NA} mesh and connects the paths.
#' "keep_na" by default.
#'
#' @export
mesh_distance <- function(mesh, mesh_to,
                          close = F,
                          type = "keep_na") {
  if (is_mesh(mesh)) {
    stopifnot(is_mesh(mesh_to))

    size <- mesh_size(mesh)
    stopifnot(size == mesh_size(mesh_to))

    mesh <- tibble::tibble(diff_n_X = field(mesh_to, "n_X") - field(mesh, "n_X"),
                           n_Y = field(mesh, "n_Y"),
                           n_Y_to = field(mesh_to, "n_Y"))

    length_X <- size / 80000
    length_Y <- length_X / 1.5

    distance <- vec_unique(mesh)
    distance <- vec_slice(distance,
                          !is.na(distance$diff_n_X) &
                            !is.na(distance$n_Y) &
                            !is.na(distance$n_Y_to))

    diff_X <- length_X * distance$diff_n_X
    Y <- length_Y * (distance$n_Y + .5)
    Y_to <- length_Y * (distance$n_Y_to + .5)

    distance$distance <- geosphere::distGeo(p1 = cbind(0, Y),
                                            p2 = cbind(diff_X, Y_to)) %>%
      units::set_units("m")

    mesh %>%
      dplyr::left_join(distance,
                       by = c("diff_n_X", "n_Y", "n_Y_to")) %>%
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


#' @export
mesh_to_XY <- function(mesh,
                       center = T) {
  UseMethod("mesh_to_XY")
}
#' @export
mesh_to_XY.mesh_80km <- function(mesh,
                                 center = T) {
  len_X <- length_X("80km")
  len_Y <- length_Y("80km")

  res <- tibble::tibble(X_min = field(mesh, "code_X_80km") + 100,
                        Y_min = field(mesh, "code_Y_80km") / 1.5) %>%
    dplyr::mutate(X_max = X_min + len_X,
                  Y_max = Y_min + len_Y)

  if (center) {
    res <- center_mesh(res)
  }
  res
}
#' @export
mesh_to_XY.mesh_10km <- function(mesh,
                                 center = T) {
  len_X <- length_X("10km")
  len_Y <- length_Y("10km")

  res <- NextMethod(center = F) %>%
    dplyr::select(X_min, Y_min) %>%
    dplyr::mutate(X_min = X_min + field(x, "code_X_10km") / 8,
                  Y_min = Y_min + field(x, "code_Y_10km") / 1.5 / 8,

                  X_max = X_min + len_X,
                  Y_max = Y_min + len_Y)

  if (center) {
    res <- center_mesh(res)
  }
  res
}
#' @export
mesh_to_XY.mesh_1km <- function(mesh,
                                center = T) {
  len_X <- length_X("1km")
  len_Y <- length_Y("1km")

  res <- NextMethod(center = F) %>%
    dplyr::select(X_min, Y_min) %>%
    dplyr::mutate(X_min = X_min + field(x, "code_X_1km") / 8 / 10,
                  Y_min = Y_min + field(x, "code_Y_1km") / 1.5 / 8 / 10,

                  X_max = X_min + len_X,
                  Y_max = Y_min + len_Y)

  if (center) {
    res <- center_mesh(res)
  }
  res
}
#' @export
mesh_to_XY.mesh_500m <- function(mesh,
                                 center = T) {
  len_X <- length_X("500m")
  len_Y <- length_Y("500m")

  code_500m <- field(x, "code_500m")

  res <- NextMethod(center = F) %>%
    dplyr::select(X_min, Y_min) %>%
    dplyr::mutate(X_min = X_min + code_2x2_to_X(code_500m) / 8 / 10 / 2,
                  Y_min = Y_min + code_2x2_to_Y(code_500m) / 1.5 / 8 / 10 / 2,

                  X_max = X_min + len_X,
                  Y_max = Y_min + len_Y)

  if (center) {
    res <- center_mesh(res)
  }
  res
}
#' @export
mesh_to_XY.mesh_250m <- function(mesh,
                                 center = T) {
  len_X <- length_X("250m")
  len_Y <- length_Y("250m")

  code_250m <- field(x, "code_250m")

  res <- NextMethod(center = F) %>%
    dplyr::select(X_min, Y_min) %>%
    dplyr::mutate(X_min = X_min + code_2x2_to_X(code_250m) / 8 / 10 / 2 / 2,
                  Y_min = Y_min + code_2x2_to_Y(code_250m) / 1.5 / 8 / 10 / 2 / 2,

                  X_max = X_min + len_X,
                  Y_max = Y_min + len_Y)

  if (center) {
    res <- center_mesh(res)
  }
  res
}
#' @export
mesh_to_XY.mesh_125m <- function(mesh,
                                 center = T) {
  len_X <- length_X("125m")
  len_Y <- length_Y("125m")

  code_125m <- field(x, "code_125m")

  res <- NextMethod(center = F) %>%
    dplyr::select(X_min, Y_min) %>%
    dplyr::mutate(X_min = X_min + code_2x2_to_X(code_125m) / 8 / 10 / 2 / 2 / 2,
                  Y_min = Y_min + code_2x2_to_Y(code_125m) / 1.5 / 8 / 10 / 2 / 2 / 2,

                  X_max = X_min + len_X,
                  Y_max = Y_min + len_Y)

  if (center) {
    res <- center_mesh(res)
  }
  res
}
#' @export
mesh_to_XY.mesh_100m <- function(mesh,
                                 center = T) {
  len_X <- length_X("100m")
  len_Y <- length_Y("100m")

  res <- NextMethod(center = F) %>%
    dplyr::select(X_min, Y_min) %>%
    dplyr::mutate(X_min = X_min + field(x, "code_X_100m") / 8 / 10 / 10,
                  Y_min = Y_min + field(x, "code_Y_100m") / 1.5 / 8 / 10 / 10,

                  X_max = X_min + len_X,
                  Y_max = Y_min + len_Y)

  if (center) {
    res <- center_mesh(res)
  }
  res
}

length_X <- function(size) {
  size <- size_match(size)
  switch(size,
         `80km` = 1,
         `10km` = 1 / 8,
         `1km` = 1 / 8 / 10,
         `500m` = 1 / 8 / 10 / 2,
         `250m` = 1 / 8 / 10 / 2 / 2,
         `125m` = 1 / 8 / 10 / 2 / 2 / 2,
         `100m` = 1 / 8 / 10 / 10)
}

length_Y <- function(size) {
  size <- size_match(size)
  switch(size,
         `80km` = 1 / 1.5,
         `10km` = 1 / 1.5 / 8,
         `1km` = 1 / 1.5 / 8 / 10,
         `500m` = 1 / 1.5 / 8 / 10 / 2,
         `250m` = 1 / 1.5 / 8 / 10 / 2 / 2,
         `125m` = 1 / 1.5 / 8 / 10 / 2 / 2 / 2,
         `100m` = 1 / 1.5 / 8 / 10 / 10)
}

center_mesh <- function(x) {
  x %>%
    dplyr::mutate(X_center = (X_min + X_max) / 2,
                  Y_center = (Y_min + Y_max) / 2,
                  .keep = "unused")
}

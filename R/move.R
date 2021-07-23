#' @export
mesh_move <- function(mesh, n_X, n_Y) {
  UseMethod("mesh_move")
}
#' @export
mesh_move.mesh_80km <- function(mesh, n_X, n_Y) {
  move_X <- move_mesh_80km(n_X,
                           code_80km = field(mesh, "code_X_80km"))
  move_Y <- move_mesh_80km(n_Y,
                           code_80km = field(mesh, "code_Y_80km"))

  mesh(code_Y_80km = move_Y$code_80km,
       code_X_80km = move_X$code_80km,

       size = "80km")
}
#' @export
mesh_move.mesh_10km <- function(mesh, n_X, n_Y) {
  move_X <- move_mesh_10km(n_X,
                           code_80km = field(mesh, "code_X_80km"),
                           code_10km = field(mesh, "code_X_10km"))
  move_Y <- move_mesh_10km(n_Y,
                           code_80km = field(mesh, "code_Y_80km"),
                           code_10km = field(mesh, "code_Y_10km"))

  mesh(code_Y_80km = move_Y$code_80km,
       code_X_80km = move_X$code_80km,

       code_Y_10km = move_Y$code_10km,
       code_X_10km = move_X$code_10km,

       size = "10km")
}
#' @export
mesh_move.mesh_1km <- function(mesh, n_X, n_Y) {
  move_X <- move_mesh_1km(n_X,
                          code_80km = field(mesh, "code_X_80km"),
                          code_10km = field(mesh, "code_X_10km"),
                          code_1km = field(mesh, "code_X_1km"))
  move_Y <- move_mesh_1km(n_Y,
                          code_80km = field(mesh, "code_Y_80km"),
                          code_10km = field(mesh, "code_Y_10km"),
                          code_1km = field(mesh, "code_Y_1km"))

  mesh(code_Y_80km = move_Y$code_80km,
       code_X_80km = move_X$code_80km,

       code_Y_10km = move_Y$code_10km,
       code_X_10km = move_X$code_10km,

       code_Y_1km = move_Y$code_1km,
       code_X_1km = move_X$code_1km,

       size = "1km")
}
#' @export
mesh_move.mesh_500m <- function(mesh, n_X, n_Y) {
  move_X <- move_mesh_500m(n_X,
                           code_80km = field(mesh, "code_X_80km"),
                           code_10km = field(mesh, "code_X_10km"),
                           code_1km = field(mesh, "code_X_1km"),
                           code_500m = code_2x2_to_X(field(mesh, "code_500m")))
  move_Y <- move_mesh_500m(n_Y,
                           code_80km = field(mesh, "code_Y_80km"),
                           code_10km = field(mesh, "code_Y_10km"),
                           code_1km = field(mesh, "code_Y_1km"),
                           code_500m = code_2x2_to_Y(field(mesh, "code_500m")))

  mesh(code_Y_80km = move_Y$code_80km,
       code_X_80km = move_X$code_80km,

       code_Y_10km = move_Y$code_10km,
       code_X_10km = move_X$code_10km,

       code_Y_1km = move_Y$code_1km,
       code_X_1km = move_X$code_1km,

       code_500m = code_XY_to_2x2(code_X = move_X$code_500m,
                                  code_Y = move_Y$code_500m),

       size = "500m")
}
#' @export
mesh_move.mesh_250m <- function(mesh, n_X, n_Y) {
  move_X <- move_mesh_250m(n_X,
                           code_80km = field(mesh, "code_X_80km"),
                           code_10km = field(mesh, "code_X_10km"),
                           code_1km = field(mesh, "code_X_1km"),
                           code_500m = code_2x2_to_X(field(mesh, "code_500m")),
                           code_250m = code_2x2_to_X(field(mesh, "code_250m")))
  move_Y <- move_mesh_250m(n_Y,
                           code_80km = field(mesh, "code_Y_80km"),
                           code_10km = field(mesh, "code_Y_10km"),
                           code_1km = field(mesh, "code_Y_1km"),
                           code_500m = code_2x2_to_Y(field(mesh, "code_500m")),
                           code_250m = code_2x2_to_Y(field(mesh, "code_250m")))

  mesh(code_Y_80km = move_Y$code_80km,
       code_X_80km = move_X$code_80km,

       code_Y_10km = move_Y$code_10km,
       code_X_10km = move_X$code_10km,

       code_Y_1km = move_Y$code_1km,
       code_X_1km = move_X$code_1km,

       code_500m = code_XY_to_2x2(code_X = move_X$code_500m,
                                  code_Y = move_Y$code_500m),
       code_250m = code_XY_to_2x2(code_X = move_X$code_250m,
                                  code_Y = move_Y$code_250m),

       size = "250m")
}
#' @export
mesh_move.mesh_125m <- function(mesh, n_X, n_Y) {
  move_X <- move_mesh_125m(n_X,
                           code_80km = field(mesh, "code_X_80km"),
                           code_10km = field(mesh, "code_X_10km"),
                           code_1km = field(mesh, "code_X_1km"),
                           code_500m = code_2x2_to_X(field(mesh, "code_500m")),
                           code_250m = code_2x2_to_X(field(mesh, "code_250m")),
                           code_125m = code_2x2_to_X(field(mesh, "code_125m")))
  move_Y <- move_mesh_125m(n_Y,
                           code_80km = field(mesh, "code_Y_80km"),
                           code_10km = field(mesh, "code_Y_10km"),
                           code_1km = field(mesh, "code_Y_1km"),
                           code_500m = code_2x2_to_Y(field(mesh, "code_500m")),
                           code_250m = code_2x2_to_Y(field(mesh, "code_250m")),
                           code_125m = code_2x2_to_Y(field(mesh, "code_125m")))

  mesh(code_Y_80km = move_Y$code_80km,
       code_X_80km = move_X$code_80km,

       code_Y_10km = move_Y$code_10km,
       code_X_10km = move_X$code_10km,

       code_Y_1km = move_Y$code_1km,
       code_X_1km = move_X$code_1km,

       code_500m = code_XY_to_2x2(code_X = move_X$code_500m,
                                  code_Y = move_Y$code_500m),
       code_250m = code_XY_to_2x2(code_X = move_X$code_250m,
                                  code_Y = move_Y$code_250m),
       code_125m = code_XY_to_2x2(code_X = move_X$code_125m,
                                  code_Y = move_Y$code_125m),

       size = "125m")
}
#' @export
mesh_move.mesh_100m <- function(mesh, n_X, n_Y) {
  move_X <- move_mesh_100m(n_X,
                           code_80km = field(mesh, "code_X_80km"),
                           code_10km = field(mesh, "code_X_10km"),
                           code_1km = field(mesh, "code_X_1km"),
                           code_100m = field(mesh, "code_X_100m"))
  move_Y <- move_mesh_100m(n_Y,
                           code_80km = field(mesh, "code_Y_80km"),
                           code_10km = field(mesh, "code_Y_10km"),
                           code_1km = field(mesh, "code_Y_1km"),
                           code_100m = field(mesh, "code_100m"))

  mesh(code_Y_80km = move_Y$code_80km,
       code_X_80km = move_X$code_80km,

       code_Y_10km = move_Y$code_10km,
       code_X_10km = move_X$code_10km,

       code_Y_1km = move_Y$code_1km,
       code_X_1km = move_X$code_1km,

       code_Y_100m = move_Y$code_100m,
       code_X_100m = move_X$code_100m,

       size = "100m")
}

move_mesh_80km <- function(n, code_80km) {
  code_80km <- code_80km + n
  list(code_80km = code_80km)
}
move_mesh_10km <- function(n, code_80km, code_10km) {
  n <- code_10km + n
  code_10km <- n %% 8
  n <- n %/% 8

  move_mesh_80km(n, code_80km) %>%
    c(list(code_10km = code_10km))
}
move_mesh_1km <- function(n, code_80km, code_10km, code_1km) {
  n <- code_1km + n
  code_1km <- n %% 10
  n <- n %/% 10

  move_mesh_10km(n, code_80km, code_10km) %>%
    c(list(code_1km = code_1km))
}
move_mesh_500m <- function(n, code_80km, code_10km, code_1km, code_500m) {
  n <- code_500m + n
  code_500m <- n %% 2
  n <- n %/% 2

  move_mesh_1km(n, code_80km, code_10km, code_1km) %>%
    c(list(code_500m = code_500m))
}
move_mesh_250m <- function(n, code_80km, code_10km, code_1km, code_500m, code_250m) {
  n <- code_250m + n
  code_250m <- n %% 2
  n <- n %/% 2

  move_mesh_500m(n, code_80km, code_10km, code_1km, code_500m) %>%
    c(list(code_250m = code_250m))
}
move_mesh_125m <- function(n, code_80km, code_10km, code_1km, code_500m, code_250m, code_125m) {
  n <- code_125m + n
  code_125m <- n %% 2
  n <- n %/% 2

  move_mesh_250m(n, code_80km, code_10km, code_1km, code_500m, code_250m) %>%
    c(list(code_250m = code_250m))
}
move_mesh_100m <- function(n, code_80km, code_10km, code_1km, code_100m) {
  n <- code_100m + n
  code_100m <- n %% 10
  n <- n %/% 10

  move_mesh_1km(n, code_80km, code_10km, code_1km) %>%
    c(list(code_100m = code_100m))
}

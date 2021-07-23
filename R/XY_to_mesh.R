XY_to_mesh <- function(X, Y, size) {
  size <- size_match(size)

  Y <- Y * 1.5
  code_Y_80km <- floor(Y)
  Y <- Y - code_Y_80km

  X <- X - 100
  code_X_80km <- floor(X)
  X <- X - code_X_80km

  if (size == "80km") {
    mesh(code_Y_80km = code_Y_80km,
         code_X_80km = code_X_80km,

         size = "80km")
  } else {
    Y <- Y * 8
    code_Y_10km <- floor(Y)
    Y <- Y - code_Y_10km

    X <- X * 8
    code_X_10km <- floor(X)
    X <- X - code_X_10km

    if (size == "10km") {
      mesh(code_Y_80km = code_Y_80km,
           code_X_80km = code_X_80km,

           code_Y_10km = code_Y_10km,
           code_X_10km = code_X_10km,

           size = "10km")
    } else {
      Y <- Y * 10
      code_Y_1km <- floor(Y)
      Y <- Y - code_Y_1km

      X <- X * 10
      code_X_1km <- floor(X)
      X <- X - code_X_1km

      if (size == "1km") {
        mesh(code_Y_80km = code_Y_80km,
             code_X_80km = code_X_80km,

             code_Y_10km = code_Y_10km,
             code_X_10km = code_X_10km,

             code_Y_1km = code_Y_1km,
             code_X_1km = code_X_1km,

             size = "1km")
      } else if (size %in% c("500m", "250m", "125m")) {
        Y <- Y * 2
        code_Y_500m <- floor(Y)
        Y <- Y - code_Y_500m

        X <- X * 2
        code_X_500m <- floor(X)
        X <- X - code_X_500m

        code_500m <- code_YX_to_2x2(code_Y = code_Y_500m,
                                    code_X = code_X_500m)

        if (size == "500m") {
          mesh(code_Y_80km = code_Y_80km,
               code_X_80km = code_X_80km,

               code_Y_10km = code_Y_10km,
               code_X_10km = code_X_10km,

               code_Y_1km = code_Y_1km,
               code_X_1km = code_X_1km,

               code_500m = code_500m,

               size = "500m")
        } else {
          Y <- Y * 2
          code_Y_250m <- floor(Y)
          Y <- Y - code_Y_250m

          X <- X * 2
          code_X_250m <- floor(X)
          X <- X - code_X_250m

          code_250m <- code_YX_to_2x2(code_Y = code_Y_250m,
                                      code_X = code_X_250m)

          if (size == "250m") {
            mesh(code_Y_80km = code_Y_80km,
                 code_X_80km = code_X_80km,

                 code_Y_10km = code_Y_10km,
                 code_X_10km = code_X_10km,

                 code_Y_1km = code_Y_1km,
                 code_X_1km = code_X_1km,

                 code_500m = code_500m,
                 code_250m = code_250m,

                 size = "250m")
          } else {
            code_Y_125m <- floor(Y * 2)
            code_X_125m <- floor(X * 2)
            code_125m <- code_YX_to_2x2(code_Y = code_Y_125m,
                                        code_X = code_X_125m)

            mesh(code_Y_80km = code_Y_80km,
                 code_X_80km = code_X_80km,

                 code_Y_10km = code_Y_10km,
                 code_X_10km = code_X_10km,

                 code_Y_1km = code_Y_1km,
                 code_X_1km = code_X_1km,

                 code_500m = code_500m,
                 code_250m = code_250m,
                 code_125m = code_125m,

                 size = "125m")
          }
        }
      } else if (size == "100m") {
        code_Y_100m <- floor(Y * 10)
        code_X_100m <- floor(X * 10)

        mesh(code_Y_80km = code_Y_80km,
             code_X_80km = code_X_80km,

             code_Y_10km = code_Y_10km,
             code_X_10km = code_X_10km,

             code_Y_1km = code_Y_1km,
             code_X_1km = code_X_1km,

             code_Y_100m = code_Y_100m,
             code_X_100m = code_X_100m,

             size = "100m")
      }
    }
  }
}

code_YX_to_2x2 <- function(code_Y, code_X) {
  dplyr::case_when(code_Y == 0 & code_X == 0 ~ 1L,
                   code_Y == 0 & code_X == 1 ~ 2L,
                   code_Y == 1 & code_X == 0 ~ 3L,
                   code_Y == 1 & code_X == 1 ~ 4L)
}

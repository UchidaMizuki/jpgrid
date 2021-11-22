test_that("XY_1km", {
  mesh1km <- mesh_1km("53394644")
  expect_equal(XY_to_mesh(139.80625, 35.704166666666666, size = "1km"), mesh1km)
  XY <- mesh_to_XY(mesh1km)
  expect_equal(XY$X, 139.80625)
  expect_equal(XY$Y, 35.704166666666666)
})

test_that("XY_reconversion", {
  x <- c(53394526313, 5339358633, 533945764, 53394611, 523503, 5339)

  for (size in c("80km", "10km", "1km", "500m", "250m", "125m", "100m")) {
    size <- size_match(size)

    mesh <- x %>%
      mesh_impl(size = size,
                strict = FALSE)
    XY <- mesh_to_XY(mesh)
    mesh1 <- XY_to_mesh(XY$X, XY$Y,
                        size = size)

    expect_equal(mesh, mesh1)
  }
})

test_that("XY_reconversion_notcenter", {
  x <- c(53394526313, 5339358633, 533945764, 53394611, 523503, 5339)
  eps <- .Machine$double.eps ^ 0.5

  for (size in c("80km", "10km", "1km", "500m", "250m", "125m", "100m")) {
    size <- size_match(size)

    mesh <- x %>%
      mesh_impl(size = size,
                strict = FALSE)
    XY <- mesh_to_XY(mesh,
                     center = FALSE)
    mesh1 <- XY_to_mesh(XY$X_min + eps, XY$Y_min + eps,
                        size = size)
    mesh2 <- XY_to_mesh(XY$X_max - eps, XY$Y_min + eps,
                        size = size)
    mesh3 <- XY_to_mesh(XY$X_min + eps, XY$Y_max - eps,
                        size = size)
    mesh4 <- XY_to_mesh(XY$X_max - eps, XY$Y_max - eps,
                        size = size)
    expect_equal(mesh, mesh1)
    expect_equal(mesh, mesh2)
    expect_equal(mesh, mesh3)
    expect_equal(mesh, mesh4)
  }
})

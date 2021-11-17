test_that("XY_1km", {
  mesh1km <- mesh_1km("53394644")
  expect_equal(XY_to_mesh(139.80625, 35.704166666666666, size = "1km"), mesh1km)
  XY <- mesh_to_XY(mesh1km)
  expect_equal(XY$X, 139.80625)
  expect_equal(XY$Y, 35.704166666666666)
})

test_that("XY_reconversion", {
  x <- c(53394526313, 5339358633, 533945764, 53394611, 523503, 5339)

  for (size in c("80km", "10km", "1km", "500m", "250m", "125m")) {
    size <- size_match(size)

    mesh1 <- x %>%
      mesh_impl(size = size,
                strict = FALSE)
    XY <- mesh_to_XY(mesh1)
    mesh2 <- XY_to_mesh(XY$X, XY$Y,
                        size = size)

    expect_equal(mesh1, mesh2)
  }
})

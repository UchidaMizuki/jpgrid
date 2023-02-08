test_that("XY_1km", {
  grid1km <- grid_parse("53394644",
                        size = "1km")
  expect_equal(grid_from_XY(139.80625, 35.704166666666666, size = "1km"), grid1km)
  XY <- grid_to_XY(grid1km)
  expect_equal(XY$X, 139.80625)
  expect_equal(XY$Y, 35.704166666666666)
})

test_that("XY_reconversion", {
  x <- c(53394526313, 5339358633, 533945764, 53394611, 523503, 5339)

  for (size in c("80km", "10km", "1km", "500m", "250m", "125m", "100m")) {
    size <- grid_size_match(size)

    grid <- grid_parse(x,
                       size = size,
                       strict = FALSE)
    XY <- grid_to_XY(grid)
    grid1 <- grid_from_XY(XY$X, XY$Y,
                          size = size)

    expect_equal(grid, grid1)
  }
})

test_that("XY_reconversion_notcenter", {
  x <- c(53394526313, 5339358633, 533945764, 53394611, 523503, 5339)
  eps <- .Machine$double.eps ^ 0.5

  for (size in c("80km", "10km", "1km", "500m", "250m", "125m", "100m")) {
    size <- grid_size_match(size)

    grid <- grid_parse(x,
                       size = size,
                       strict = FALSE)
    XY <- grid_to_XY(grid,
                     center = FALSE)
    grid1 <- grid_from_XY(XY$X_min + eps, XY$Y_min + eps,
                          size = size)
    grid2 <- grid_from_XY(XY$X_max - eps, XY$Y_min + eps,
                          size = size)
    grid3 <- grid_from_XY(XY$X_min + eps, XY$Y_max - eps,
                          size = size)
    grid4 <- grid_from_XY(XY$X_max - eps, XY$Y_max - eps,
                          size = size)
    expect_equal(grid, grid1)
    expect_equal(grid, grid2)
    expect_equal(grid, grid3)
    expect_equal(grid, grid4)
  }
})

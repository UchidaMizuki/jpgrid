test_that("XY_1km", {
  grid1km <- grid_1km("53394644")
  expect_equal(XY_to_grid(139.80625, 35.704166666666666, size = "1km"), grid1km)
  XY <- grid_to_XY(grid1km)
  expect_equal(XY$X, 139.80625)
  expect_equal(XY$Y, 35.704166666666666)
})

test_that("XY_reconversion", {
  x <- c(53394526313, 5339358633, 533945764, 53394611, 523503, 5339)

  for (size in c("80km", "10km", "1km", "500m", "250m", "125m", "100m")) {
    size <- size_match(size)

    grid <- grid_parse(x,
                       size = size,
                       strict = FALSE)
    XY <- grid_to_XY(grid)
    grid1 <- XY_to_grid(XY$X, XY$Y,
                        size = size)

    expect_equal(grid, grid1)
  }
})

test_that("XY_reconversion_notcenter", {
  x <- c(53394526313, 5339358633, 533945764, 53394611, 523503, 5339)
  eps <- .Machine$double.eps ^ 0.5

  for (size in c("80km", "10km", "1km", "500m", "250m", "125m", "100m")) {
    size <- size_match(size)

    grid <- grid_parse(x,
                       size = size,
                       strict = FALSE)
    XY <- grid_to_XY(grid,
                     center = FALSE)
    grid1 <- XY_to_grid(XY$X_min + eps, XY$Y_min + eps,
                        size = size)
    grid2 <- XY_to_grid(XY$X_max - eps, XY$Y_min + eps,
                        size = size)
    grid3 <- XY_to_grid(XY$X_min + eps, XY$Y_max - eps,
                        size = size)
    grid4 <- XY_to_grid(XY$X_max - eps, XY$Y_max - eps,
                        size = size)
    expect_equal(grid, grid1)
    expect_equal(grid, grid2)
    expect_equal(grid, grid3)
    expect_equal(grid, grid4)
  }
})

test_that("coords_1km", {
  grid1km <- grid_parse("53394644",
                        size = "1km")
  expect_equal(grid_from_coords(139.80625, 35.704166666666666, size = "1km"), grid1km)
  coords <- grid_to_coords(grid1km)
  expect_equal(coords$X, 139.80625)
  expect_equal(coords$Y, 35.704166666666666)
})

test_that("coords_reconversion", {
  x <- c(53394526313, 5339358633, 533945764, 53394611, 523503, 5339)

  for (size in c("80km", "10km", "1km", "500m", "250m", "125m", "100m")) {
    size <- grid_size_match(size)

    grid <- grid_parse(x,
                       size = size,
                       strict = FALSE)
    coords <- grid_to_coords(grid)
    grid1 <- grid_from_coords(coords$X, coords$Y,
                              size = size)

    expect_equal(grid, grid1)
  }
})

test_that("coords_reconversion_notcenter", {
  x <- c(53394526313, 5339358633, 533945764, 53394611, 523503, 5339)
  eps <- .Machine$double.eps ^ 0.5

  for (size in c("80km", "10km", "1km", "500m", "250m", "125m", "100m")) {
    size <- grid_size_match(size)

    grid <- grid_parse(x,
                       size = size,
                       strict = FALSE)
    coords <- grid_to_coords(grid,
                             center = FALSE)
    grid1 <- grid_from_coords(coords$X_min + eps, coords$Y_min + eps,
                              size = size)
    grid2 <- grid_from_coords(coords$X_max - eps, coords$Y_min + eps,
                              size = size)
    grid3 <- grid_from_coords(coords$X_min + eps, coords$Y_max - eps,
                              size = size)
    grid4 <- grid_from_coords(coords$X_max - eps, coords$Y_max - eps,
                              size = size)
    expect_equal(grid, grid1)
    expect_equal(grid, grid2)
    expect_equal(grid, grid3)
    expect_equal(grid, grid4)
  }
})

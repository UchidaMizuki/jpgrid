test_that("coords_1km", {
  grid1km <- grid_parse("53394644", "1km")
  expect_equal(grid_from_coords(139.80625, 35.704166666666666, "1km"), grid1km)
  coords <- grid_to_coords(grid1km)
  expect_equal(coords$X, 139.80625)
  expect_equal(coords$Y, 35.704166666666666)
})

test_that("coords_reconversion", {
  x <- c(53394526313, 5339358633, 533945764, 53394611, 523503, 5339)

  for (grid_size in c("80km", "10km", "1km", "500m", "250m", "125m", "100m")) {
    grid_size <- grid_size_match(grid_size)

    grid <- grid_parse(x, grid_size,
                       strict = FALSE)
    coords <- grid_to_coords(grid)
    grid1 <- grid_from_coords(coords$X, coords$Y, grid_size)

    expect_equal(grid, grid1)
  }
})

test_that("coords_reconversion_notcenter", {
  x <- c(53394526313, 5339358633, 533945764, 53394611, 523503, 5339)
  eps <- .Machine$double.eps ^ 0.5

  for (grid_size in c("80km", "10km", "1km", "500m", "250m", "125m", "100m")) {
    grid_size <- grid_size_match(grid_size)

    grid <- grid_parse(x,
                       grid_size = grid_size,
                       strict = FALSE)
    coords <- grid_to_coords(grid,
                             center = FALSE)
    grid1 <- grid_from_coords(coords$X_min + eps, coords$Y_min + eps,
                              grid_size = grid_size)
    grid2 <- grid_from_coords(coords$X_max - eps, coords$Y_min + eps,
                              grid_size = grid_size)
    grid3 <- grid_from_coords(coords$X_min + eps, coords$Y_max - eps,
                              grid_size = grid_size)
    grid4 <- grid_from_coords(coords$X_max - eps, coords$Y_max - eps,
                              grid_size = grid_size)
    expect_equal(grid, grid1)
    expect_equal(grid, grid2)
    expect_equal(grid, grid3)
    expect_equal(grid, grid4)
  }
})

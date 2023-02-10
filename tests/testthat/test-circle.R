test_that("circle", {
  # 1km
  grid_1km <- parse_grid("53396354", "1km")
  coords <- grid_to_coords(grid_1km)

  grid_100m <- grid_circle(X = coords$X,
                           Y = coords$Y,
                           dist = units::set_units(1, km),
                           grid_size = "100m")

  expect_true(is.list(grid_100m))
  expect_true(is_grid(grid_100m[[1L]], "100m"))
})

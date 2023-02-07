test_that("subdivide", {
  # 1km
  grid_1km <- grid_parse("53396354",
                         size = "1km")

  # 1km -> 1km
  expect_equal(grid_subdivide(grid_1km, size = "1km")[[1]], grid_1km)
  expect_equal(grid_subdivide(grid_1km, size = 1000)[[1]], grid_1km)
  expect_equal(grid_subdivide(grid_1km, size = units::set_units(1, km))[[1]], grid_1km)

  # 1km -> 500m
  expect_true(setequal(as.character(grid_subdivide(grid_1km, size = "500m")[[1]]),
                       stringr::str_c(grid_1km, 1:4)))
})

test_that("zoomout", {
  # 1km
  grid_1km <- grid_parse("53396354",
                         size = "1km")

  # 1km -> 1km
  expect_equal(grid_convert(grid_1km, size = "1km"), grid_1km)
  # 1km -> 10km
  expect_equal(as.character(grid_convert(grid_1km, size = "10km")), stringr::str_sub(grid_1km, 1, 6))
})

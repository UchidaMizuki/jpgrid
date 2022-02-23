test_that("subdivide", {
  # 1km
  grid1km <- grid_1km("53396354")

  # 1km -> 1km
  expect_equal(grid_subdivide(grid1km, size = "1km")[[1]], grid1km)
  expect_equal(grid_subdivide(grid1km, size = 1000)[[1]], grid1km)
  expect_equal(grid_subdivide(grid1km, size = units::set_units(1, km))[[1]], grid1km)

  # 1km -> 500m
  expect_true(setequal(as.character(grid_subdivide(grid1km, size = "500m")[[1]]),
                       stringr::str_c(grid1km, 1:4)))
})

test_that("zoomout", {
  # 1km
  grid1km <- grid_1km("53396354")

  # 1km -> 1km
  expect_equal(grid_1km(grid1km), grid1km)
  # 1km -> 10km
  expect_equal(as.character(grid_10km(grid1km)), stringr::str_sub(grid1km, 1, 6))
})

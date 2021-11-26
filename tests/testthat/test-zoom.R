test_that("subdivide", {
  # 1km
  mesh1km <- mesh_1km("53396354")

  # 1km -> 1km
  expect_equal(mesh_subdivide(mesh1km, size = "1km")[[1]], mesh1km)
  expect_equal(mesh_subdivide(mesh1km, size = 1000)[[1]], mesh1km)
  expect_equal(mesh_subdivide(mesh1km, size = units::set_units(1, km))[[1]], mesh1km)

  # 1km -> 500m
  expect_true(setequal(as.character(mesh_subdivide(mesh1km, size = "500m")[[1]]),
                       stringr::str_c(mesh1km, 1:4)))
})

test_that("zoomout", {
  # 1km
  mesh1km <- mesh_1km("53396354")

  # 1km -> 1km
  expect_equal(mesh_1km(mesh1km), mesh1km)
  # 1km -> 10km
  expect_equal(as.character(mesh_10km(mesh1km)), stringr::str_sub(mesh1km, 1, 6))
})

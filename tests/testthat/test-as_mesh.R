test_that("as_mesh", {
  x <- c("53394526313", 5339358633, "533945764", 53394611, "523503", 5339)

  mesh_80km <- as_mesh(x)
  mesh_10km <- as_mesh(x, size = "10km")
  mesh_1km <- as_mesh(x, size = "1km")
  mesh_500m <- as_mesh(x, size = "500m")
  mesh_250m <- as_mesh(x, size = "250m")
  mesh_125m <- as_mesh(x, size = "125m")

  expect_equal(as.character(mesh_80km),
               stringr::str_extract(x, "^\\d{4}"))
  expect_equal(as.character(mesh_10km),
               stringr::str_extract(x, "^\\d{6}"))
  expect_equal(as.character(mesh_1km),
               stringr::str_extract(x, "^\\d{8}"))
  expect_equal(as.character(mesh_500m),
               stringr::str_extract(x, "^\\d{9}"))
  expect_equal(as.character(mesh_250m),
               stringr::str_extract(x, "^\\d{10}"))
  expect_equal(as.character(mesh_125m),
               stringr::str_extract(x, "^\\d{11}"))
})

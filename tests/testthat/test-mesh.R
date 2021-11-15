test_that("mesh", {
  x <- c("53394526313", 5339358633, "533945764", 53394611, "523503", 5339)

  mesh80km <- mesh_80km(x)
  mesh10km <- mesh_10km(x)
  mesh1km <- mesh_1km(x)
  mesh500m <- mesh_500m(x)
  mesh250m <- mesh_250m(x)
  mesh125m <- mesh_125m(x)

  expect_equal(as.character(mesh80km),
               stringr::str_extract(x, "^\\d{4}"))
  expect_equal(as.character(mesh10km),
               stringr::str_extract(x, "^\\d{6}"))
  expect_equal(as.character(mesh1km),
               stringr::str_extract(x, "^\\d{8}"))
  expect_equal(as.character(mesh500m),
               stringr::str_extract(x, "^\\d{9}"))
  expect_equal(as.character(mesh250m),
               stringr::str_extract(x, "^\\d{10}"))
  expect_equal(as.character(mesh125m),
               stringr::str_extract(x, "^\\d{11}"))
})

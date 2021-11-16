test_that("mesh_strict", {
  x <- c("53394526313", 5339358633, "533945764", 53394611, "523503", 5339)

  # strict
  mesh80km <- mesh_80km(x)
  mesh10km <- mesh_10km(x)
  mesh1km <- mesh_1km(x)
  mesh500m <- mesh_500m(x)
  mesh250m <- mesh_250m(x)
  mesh125m <- mesh_125m(x)
  mesh100m <- mesh_100m(x)

  expect_s3_class(mesh80km, "mesh_80km")
  expect_s3_class(mesh10km, "mesh_10km")
  expect_s3_class(mesh1km, "mesh_1km")
  expect_s3_class(mesh500m, "mesh_500m")
  expect_s3_class(mesh250m, "mesh_250m")
  expect_s3_class(mesh125m, "mesh_125m")
  expect_s3_class(mesh100m, "mesh_100m")

  expect_equal(as.character(mesh80km),
               stringr::str_extract(x, "^\\d{4}$"))
  expect_equal(as.character(mesh10km),
               stringr::str_extract(x, "^\\d{6}$"))
  expect_equal(as.character(mesh1km),
               stringr::str_extract(x, "^\\d{8}$"))
  expect_equal(as.character(mesh500m),
               stringr::str_extract(x, "^\\d{9}$"))
  expect_equal(as.character(mesh250m),
               stringr::str_extract(x, "^\\d{10}$"))
  expect_equal(as.character(mesh125m),
               stringr::str_extract(x, "^\\d{11}$"))
  expect_equal(as.character(mesh100m),
               stringr::str_extract(x, "^\\d{10}$"))
})

test_that("mesh_notstrict", {
  x <- c("53394526313", 5339358633, "533945764", 53394611, "523503", 5339)

  # not strict
  mesh80km <- mesh_80km(x, strict = F)
  mesh10km <- mesh_10km(x, strict = F)
  mesh1km <- mesh_1km(x, strict = F)
  mesh500m <- mesh_500m(x, strict = F)
  mesh250m <- mesh_250m(x, strict = F)
  mesh125m <- mesh_125m(x, strict = F)
  mesh100m <- mesh_100m(x, strict = F)

  expect_s3_class(mesh80km, "mesh_80km")
  expect_s3_class(mesh10km, "mesh_10km")
  expect_s3_class(mesh1km, "mesh_1km")
  expect_s3_class(mesh500m, "mesh_500m")
  expect_s3_class(mesh250m, "mesh_250m")
  expect_s3_class(mesh125m, "mesh_125m")
  expect_s3_class(mesh100m, "mesh_100m")

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
  expect_equal(as.character(mesh100m),
               stringr::str_extract(x, "^\\d{10}"))
})

test_that("mesh_auto", {
  x <- c("53394526313", 5339358633, "533945764", 53394611, "523503", 5339)

  expect_s3_class(mesh_auto(x[1]), "mesh_125m")
  expect_s3_class(mesh_auto(x[1:2]), "mesh_250m")
  expect_s3_class(mesh_auto(x[1:3]), "mesh_500m")
  expect_s3_class(mesh_auto(x[1:4]), "mesh_1km")
  expect_s3_class(mesh_auto(x[1:5]), "mesh_10km")
  expect_s3_class(mesh_auto(x[1:6]), "mesh_80km")
})

test_that("mesh_250m_or_100m", {
  x <- "5339452619"
  expect_true(is.na(mesh_250m(x)))
  expect_equal(as.character(mesh_100m(x)), x)
})

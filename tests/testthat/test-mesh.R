test_that("grid_strict", {
  x <- c("53394526313", 5339358633, "533945764", 53394611, "523503", 5339)

  # strict
  grid80km <- grid_80km(x)
  grid10km <- grid_10km(x)
  grid1km <- grid_1km(x)
  grid500m <- grid_500m(x)
  grid250m <- grid_250m(x)
  grid125m <- grid_125m(x)
  grid100m <- grid_100m(x)

  expect_s3_class(grid80km, "grid_80km")
  expect_s3_class(grid10km, "grid_10km")
  expect_s3_class(grid1km, "grid_1km")
  expect_s3_class(grid500m, "grid_500m")
  expect_s3_class(grid250m, "grid_250m")
  expect_s3_class(grid125m, "grid_125m")
  expect_s3_class(grid100m, "grid_100m")

  expect_equal(as.character(grid80km),
               stringr::str_extract(x, "^\\d{4}$"))
  expect_equal(as.character(grid10km),
               stringr::str_extract(x, "^\\d{6}$"))
  expect_equal(as.character(grid1km),
               stringr::str_extract(x, "^\\d{8}$"))
  expect_equal(as.character(grid500m),
               stringr::str_extract(x, "^\\d{9}$"))
  expect_equal(as.character(grid250m),
               stringr::str_extract(x, "^\\d{10}$"))
  expect_equal(as.character(grid125m),
               stringr::str_extract(x, "^\\d{11}$"))
  expect_equal(as.character(grid100m),
               stringr::str_extract(x, "^\\d{10}$"))
})

test_that("grid_notstrict", {
  x <- c("53394526313", 5339358633, "533945764", 53394611, "523503", 5339)

  # not strict
  grid80km <- grid_80km(x, strict = F)
  grid10km <- grid_10km(x, strict = F)
  grid1km <- grid_1km(x, strict = F)
  grid500m <- grid_500m(x, strict = F)
  grid250m <- grid_250m(x, strict = F)
  grid125m <- grid_125m(x, strict = F)
  grid100m <- grid_100m(x, strict = F)

  expect_s3_class(grid80km, "grid_80km")
  expect_s3_class(grid10km, "grid_10km")
  expect_s3_class(grid1km, "grid_1km")
  expect_s3_class(grid500m, "grid_500m")
  expect_s3_class(grid250m, "grid_250m")
  expect_s3_class(grid125m, "grid_125m")
  expect_s3_class(grid100m, "grid_100m")

  expect_equal(as.character(grid80km),
               stringr::str_extract(x, "^\\d{4}"))
  expect_equal(as.character(grid10km),
               stringr::str_extract(x, "^\\d{6}"))
  expect_equal(as.character(grid1km),
               stringr::str_extract(x, "^\\d{8}"))
  expect_equal(as.character(grid500m),
               stringr::str_extract(x, "^\\d{9}"))
  expect_equal(as.character(grid250m),
               stringr::str_extract(x, "^\\d{10}"))
  expect_equal(as.character(grid125m),
               stringr::str_extract(x, "^\\d{11}"))
  expect_equal(as.character(grid100m),
               stringr::str_extract(x, "^\\d{10}"))
})

test_that("grid_auto", {
  x <- c("53394526313", 5339358633, "533945764", 53394611, "523503", 5339)

  expect_s3_class(grid_auto(x[1]), "grid_125m")
  expect_s3_class(grid_auto(x[1:2]), "grid_250m")
  expect_s3_class(grid_auto(x[1:3]), "grid_500m")
  expect_s3_class(grid_auto(x[1:4]), "grid_1km")
  expect_s3_class(grid_auto(x[1:5]), "grid_10km")
  expect_s3_class(grid_auto(x[1:6]), "grid_80km")
})

test_that("grid_250m_or_100m", {
  x <- "5339452619"
  expect_true(is.na(grid_250m(x)))
  expect_equal(as.character(grid_100m(x)), x)
})

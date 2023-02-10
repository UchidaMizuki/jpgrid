test_that("grid_strict", {
  x <- c("53394526313", 5339358633, "533945764", 53394611, "523503", 5339)

  # strict
  grid_80km <- parse_grid(x, "80km")
  grid_10km <- parse_grid(x, "10km")
  grid_1km <- parse_grid(x, "1km")
  grid_500m <- parse_grid(x, "500m")
  grid_250m <- parse_grid(x, "250m")
  grid_125m <- parse_grid(x, "125m")
  grid_100m <- parse_grid(x, "100m")

  expect_s3_class(grid_80km, "grid_80km")
  expect_s3_class(grid_10km, "grid_10km")
  expect_s3_class(grid_1km, "grid_1km")
  expect_s3_class(grid_500m, "grid_500m")
  expect_s3_class(grid_250m, "grid_250m")
  expect_s3_class(grid_125m, "grid_125m")
  expect_s3_class(grid_100m, "grid_100m")

  expect_equal(as.character(grid_80km),
               stringr::str_extract(x, "^\\d{4}$"))
  expect_equal(as.character(grid_10km),
               stringr::str_extract(x, "^\\d{6}$"))
  expect_equal(as.character(grid_1km),
               stringr::str_extract(x, "^\\d{8}$"))
  expect_equal(as.character(grid_500m),
               stringr::str_extract(x, "^\\d{9}$"))
  expect_equal(as.character(grid_250m),
               stringr::str_extract(x, "^\\d{10}$"))
  expect_equal(as.character(grid_125m),
               stringr::str_extract(x, "^\\d{11}$"))
  expect_equal(as.character(grid_100m),
               stringr::str_extract(x, "^\\d{10}$"))
})

test_that("grid_notstrict", {
  x <- c("53394526313", 5339358633, "533945764", 53394611, "523503", 5339)

  # not strict
  grid_80km <- parse_grid(x, "80km",
                          strict = F)
  grid_10km <- parse_grid(x, "10km",
                          strict = F)
  grid_1km <- parse_grid(x, "1km",
                         strict = F)
  grid_500m <- parse_grid(x, "500m",
                          strict = F)
  grid_250m <- parse_grid(x, "250m",
                          strict = F)
  grid_125m <- parse_grid(x, "125m",
                          strict = F)
  grid_100m <- parse_grid(x, "100m",
                          strict = F)

  expect_s3_class(grid_80km, "grid_80km")
  expect_s3_class(grid_10km, "grid_10km")
  expect_s3_class(grid_1km, "grid_1km")
  expect_s3_class(grid_500m, "grid_500m")
  expect_s3_class(grid_250m, "grid_250m")
  expect_s3_class(grid_125m, "grid_125m")
  expect_s3_class(grid_100m, "grid_100m")

  expect_equal(as.character(grid_80km),
               stringr::str_extract(x, "^\\d{4}"))
  expect_equal(as.character(grid_10km),
               stringr::str_extract(x, "^\\d{6}"))
  expect_equal(as.character(grid_1km),
               stringr::str_extract(x, "^\\d{8}"))
  expect_equal(as.character(grid_500m),
               stringr::str_extract(x, "^\\d{9}"))
  expect_equal(as.character(grid_250m),
               stringr::str_extract(x, "^\\d{10}"))
  expect_equal(as.character(grid_125m),
               stringr::str_extract(x, "^\\d{11}"))
  expect_equal(as.character(grid_100m),
               stringr::str_extract(x, "^\\d{10}"))
})

test_that("grid_auto", {
  x <- c("53394526313", 5339358633, "533945764", 53394611, "523503", 5339)

  expect_s3_class(parse_grid(x[1]), "grid_125m")
  expect_s3_class(parse_grid(x[1:2]), "grid_250m")
  expect_s3_class(parse_grid(x[1:3]), "grid_500m")
  expect_s3_class(parse_grid(x[1:4]), "grid_1km")
  expect_s3_class(parse_grid(x[1:5]), "grid_10km")
  expect_s3_class(parse_grid(x[1:6]), "grid_80km")
})

test_that("grid_250m_or_100m", {
  x <- "5339452619"
  expect_true(is.na(parse_grid(x, "250m")))
  expect_equal(as.character(parse_grid(x, "100m")), x)
})

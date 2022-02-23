test_that("polygon_to_grid", {
  polygon <- sf::st_polygon(list(rbind(c(139.55625, 35.62083333333334),
                                       c(139.58125, 35.62916666666666),
                                       c(139.56875, 35.64583333333333),
                                       c(139.55625, 35.62083333333334)))) %>%
    sf::st_sfc()
  polygon <- rep(polygon, 3L)

  grid <- geometry_to_grid(polygon,
                           size = "500m")
  expect_s3_class(grid[[1L]], "grid")
})

test_that("point_to_grid", {
  point <- sf::st_multipoint(rbind(c(139.55625, 35.62083333333334),
                                   c(139.58125, 35.62916666666666),
                                   c(139.56875, 35.64583333333333),
                                   c(139.55625, 35.62083333333334))) %>%
    sf::st_sfc() %>%
    sf::st_cast("POINT")

  grid <- geometry_to_grid(point,
                           size = "1km")
  expect_s3_class(grid[[1L]], "grid")
})

test_that("st_as_sfc", {
  grid10km <- c("53394526313", 5339358633, "533945764", 53394611, "523503", 5339) %>%
    grid_10km()

  expect_s3_class(st_as_sfc(grid10km, as_points = TRUE), "sfc_POINT")
  expect_s3_class(st_as_sfc(grid10km), "sfc_POLYGON")
})

test_that("bbox_to_grid", {
  bbox <- sf::st_bbox(c(xmin = 139.80625,
                        ymin = 35.712500000000006,
                        xmax = 139.84375,
                        ymax = 35.72916666666667))

  grid <- bbox_to_grid(bbox,
                       size = "1km")
  expect_equal(vctrs::vec_size(grid), 12L)

  # grid <- bbox_to_grid(list(bbox, bbox),
  #                      size = "1km")
  # expect_true(is.list(grid))
  # expect_equal(vctrs::vec_size(grid[[1L]]), 12L)
})

test_that("st_as_sf-2", {
  grid10km_1 <- c("53394526313", 5339358633, "533945764", 53394611, "523503", 5339) %>%
    grid_10km(strict = FALSE)
  grid10km_2 <- rev(grid10km_1)

  sf <- tibble::tibble(grid1 = grid10km_1,
                       grid2 = grid10km_2) %>%
    sf::st_as_sf()
  expect_s3_class(sf, "sf")
})

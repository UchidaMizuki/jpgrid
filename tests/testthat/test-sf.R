test_that("polygon_to_grid", {
  polygon <- sf::st_polygon(list(rbind(c(139.55625, 35.62083333333334),
                                       c(139.58125, 35.62916666666666),
                                       c(139.56875, 35.64583333333333),
                                       c(139.55625, 35.62083333333334)))) |>
    sf::st_sfc()
  polygon <- rep(polygon, 3L)

  grid <- geometry_to_grid(polygon, "500m")
  expect_s3_class(grid[[1L]], "grid")
})

test_that("point_to_grid", {
  point <- sf::st_multipoint(rbind(c(139.55625, 35.62083333333334),
                                   c(139.58125, 35.62916666666666),
                                   c(139.56875, 35.64583333333333),
                                   c(139.55625, 35.62083333333334))) |>
    sf::st_sfc() |>
    sf::st_cast("POINT")

  grid <- geometry_to_grid(point, "1km")
  expect_s3_class(grid[[1L]], "grid")
})

test_that("st_as_sfc", {
  grid_10km <- parse_grid(c("53394526313", 5339358633, "533945764", 53394611, "523503", 5339), "10km")

  expect_s3_class(st_as_sfc(grid_10km, as_points = TRUE), "sfc_POINT")
  expect_s3_class(st_as_sfc(grid_10km), "sfc_POLYGON")
})

test_that("bbox_to_grid", {
  bbox <- sf::st_bbox(c(xmin = 139.80625,
                        ymin = 35.712500000000006,
                        xmax = 139.84375,
                        ymax = 35.72916666666667))

  grid <- bbox_to_grid(bbox, "1km")
  expect_equal(vctrs::vec_size(grid), 12L)
})

test_that("grid_as_sf", {
  grid_10km_1 <- parse_grid(c("53394526313", 5339358633, "533945764", 53394611, "523503", 5339), "10km",
                            strict = FALSE)
  grid_10km_2 <- rev(grid_10km_1)

  sf <- tibble::tibble(grid1 = grid_10km_1,
                       grid2 = grid_10km_2) |>
    grid_as_sf()
  expect_s3_class(sf, "sf")
})

test_that("st_bbox", {
  grid <- parse_grid(c("533945263", "533935863", "533945764"), "500m")
  bbox <- st_bbox(grid)
  grid_bbox <- bbox_to_grid(bbox, "500m")

  expect_s3_class(bbox, "bbox")
  expect_true(all(vec_in(grid, grid_bbox)))
})

test_that("grid_as_sf", {
  set.seed(1234)

  grid <- parse_grid(c("533945263", "533935863", "533945764"), "500m")
  df_grid <- grid_as_sf(grid)

  expect_s3_class(df_grid, "sf")

  df_grid <- tibble::tibble(grid = grid) |>
    grid_as_sf()

  expect_s3_class(df_grid, "sf")

  grid <- parse_grid("5339",
                     grid_size = "80km") |>
    grid_subdivide(grid_size = "1km") |>
    dplyr::first() |>
    sample(size = 1e3)

  expect_equal(grid,
               st_as_sfc(grid,
                         as_points = TRUE) |>
                 geometry_to_grid(grid_size = "1km"))
  expect_equal(grid,
               st_as_sfc(grid) |>
                 sf::st_centroid() |>
                 geometry_to_grid(grid_size = "1km"))
})


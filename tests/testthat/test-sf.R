test_that("polygon_to_mesh", {
  polygon <- sf::st_polygon(list(rbind(c(139.55625, 35.62083333333334),
                                       c(139.58125, 35.62916666666666),
                                       c(139.56875, 35.64583333333333),
                                       c(139.55625, 35.62083333333334)))) %>%
    sf::st_sfc()
  polygon <- rep(polygon, 3L)

  mesh <- geometry_to_mesh(polygon,
                           size = "500m")
  expect_s3_class(mesh[[1L]], "mesh")
})

test_that("point_to_mesh", {
  point <- sf::st_multipoint(rbind(c(139.55625, 35.62083333333334),
                                   c(139.58125, 35.62916666666666),
                                   c(139.56875, 35.64583333333333),
                                   c(139.55625, 35.62083333333334))) %>%
    sf::st_sfc() %>%
    sf::st_cast("POINT")

  mesh <- geometry_to_mesh(point,
                           size = "1km")
  expect_s3_class(mesh[[1L]], "mesh")
})

test_that("mesh_as_sfc", {
  mesh10km <- c("53394526313", 5339358633, "533945764", 53394611, "523503", 5339) %>%
    mesh_10km()

  expect_s3_class(mesh_as_sfc(mesh10km, as_points = TRUE), "sfc_POINT")
  expect_s3_class(mesh_as_sfc(mesh10km), "sfc_POLYGON")
})

test_that("bbox_to_mesh", {
  bbox <- sf::st_bbox(c(xmin = 139.80625,
                        ymin = 35.712500000000006,
                        xmax = 139.84375,
                        ymax = 35.72916666666667))

  mesh <- bbox_to_mesh(bbox,
                       size = "1km")
  expect_equal(vctrs::vec_size(mesh), 12L)

  mesh <- bbox_to_mesh(list(bbox, bbox),
                       size = "1km")
  expect_true(is.list(mesh))
  expect_equal(vctrs::vec_size(mesh[[1L]]), 12L)
})

test_that("mesh_as_sf", {
  mesh10km_1 <- c("53394526313", 5339358633, "533945764", 53394611, "523503", 5339) %>%
    mesh_10km(strict = FALSE)
  mesh10km_2 <- rev(mesh10km_1)

  sf <- tibble::tibble(mesh1 = mesh10km_1,
                       mesh2 = mesh10km_2) %>%
    mesh_as_sf()
  expect_s3_class(sf, "sf")
})

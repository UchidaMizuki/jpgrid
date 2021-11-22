test_that("sfc", {
  mesh10km <- c("53394526313", 5339358633, "533945764", 53394611, "523503", 5339) %>%
    mesh_10km()

  expect_s3_class(mesh_to_point(mesh10km), "sfc_POINT")
  expect_s3_class(mesh_to_polygon(mesh10km), "sfc_POLYGON")
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

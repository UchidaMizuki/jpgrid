test_that("sfc", {
  mesh10km <- c("53394526313", 5339358633, "533945764", 53394611, "523503", 5339) %>%
    mesh_10km()

  expect_s3_class(mesh_to_point(mesh10km), "sfc_POINT")
  expect_s3_class(mesh_to_polygon(mesh10km), "sfc_POLYGON")
})

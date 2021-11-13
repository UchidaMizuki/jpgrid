test_that("sfc", {
  x <- c("53394526313", 5339358633, "533945764", 53394611, "523503", 5339) %>%
    as_mesh(x, size = "10km")

  mesh_to_point(x)
  mesh_to_polygon(x)
})

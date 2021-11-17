test_that("distance", {
  mesh_from <- mesh_80km("6441")
  mesh_to <- mesh_80km("5237")

  distance_1 <- mesh_distance(mesh_from, mesh_to) %>%
    units::drop_units()
  distance_2 <- geosphere::distGeo(p1 = cbind(141.5, 43),
                                   p2 = cbind(137.5, 35))
  expect_equal(distance_1, distance_2)
})

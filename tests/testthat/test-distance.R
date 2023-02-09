test_that("distance", {
  grid_from <- grid_parse("6441", "80km")
  grid_to <- grid_parse("5237", "80km")

  distance_1 <- grid_distance(grid_from, grid_to) |>
    units::drop_units()
  distance_2 <- geosphere::distGeo(p1 = cbind(141.5, 43),
                                   p2 = cbind(137.5, 35))
  expect_equal(distance_1, distance_2)

  distance_3 <- grid_distance(list(c(grid_from, grid_to))) |>
    dplyr::first() |>
    units::drop_units()
  expect_equal(distance_1, distance_3)
})

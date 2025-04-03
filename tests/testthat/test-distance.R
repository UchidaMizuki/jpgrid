test_that("distance", {
  grid_from <- parse_grid("6441", "80km")
  grid_to <- parse_grid("5237", "80km")

  distance_1 <- grid_distance(grid_from, grid_to)
  distance_2 <- distance_by_element(
    X_from = 141.5,
    Y_from = 43,
    X_to = 137.5,
    Y_to = 35
  )
  expect_equal(distance_1, distance_2)

  distance_3 <- grid_distance(list(c(grid_from, grid_to))) |>
    dplyr::first()
  expect_equal(distance_1, distance_3)
})

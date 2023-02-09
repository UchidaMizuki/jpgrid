test_that("bbox", {
  grid <- grid_parse(c("533945263", "533935863", "533945764"), "500m")
  grid_bbox <- grid_bbox(grid)

  expect_true(all(vec_in(grid, grid_bbox)))
})

test_that("move", {
  grid_10km <- grid_parse(533877, "10km")

  expect_equal(grid_move(grid_10km, 1, 1), grid_parse(543900, "10km"))
})

test_that("neighbor", {
  grid_10km <- grid_parse(533900, "10km")

  expect_true(setequal(as.character(grid_neighbor(grid_10km, n = 0:1)[[1]]),
                       as.character(grid_parse(c(533817, 533910, 533911,
                                                 533807, 533900, 533901,
                                                 523877, 523970, 523971),
                                               grid_size = "10km"))))
})

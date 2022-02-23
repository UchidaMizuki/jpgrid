test_that("move", {
  grid10km <- grid_10km(533877)

  expect_equal(grid_move(grid10km, 1, 1), grid_10km(543900))
})

test_that("neighbor", {
  grid10km <- grid_10km(533900)

  expect_true(setequal(as.character(grid_neighbor(grid10km, n = 0:1)[[1]]),
                       as.character(grid_10km(c(533817, 533910, 533911,
                                                533807, 533900, 533901,
                                                523877, 523970, 523971)))))
})

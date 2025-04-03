test_that("move", {
  grid_10km <- parse_grid(533877, "10km")

  expect_equal(grid_move(grid_10km, 1, 1), parse_grid(543900, "10km"))
})

test_that("neighborhood", {
  grid_10km <- parse_grid(533900, "10km")

  expect_true(
    setequal(
      as.character(
        grid_neighborhood(grid_10km, n = 0:1, type = "von_neumann")[[1]]
      ),
      as.character(parse_grid(
        c(533910, 533807, 533900, 533901, 523970),
        grid_size = "10km"
      ))
    )
  )
  expect_true(setequal(
    as.character(grid_neighborhood(grid_10km, n = 0:1, type = "moore")[[1]]),
    as.character(parse_grid(
      c(533817, 533910, 533911, 533807, 533900, 533901, 523877, 523970, 523971),
      grid_size = "10km"
    ))
  ))
})

test_that("components", {
  grid_1km <- parse_grid(
    c(53394620, 53394631, 53394632, 53394507, 53394508, 53394509),
    "1km"
  )

  expect_equal(
    grid_components(grid_1km, n = 0:1, type = "von_neumann"),
    c(3, 2, 2, 1, 1, 1)
  )
  expect_equal(
    grid_components(grid_1km, n = 0:1, type = "moore"),
    c(1, 1, 1, 2, 2, 2)
  )
  expect_equal(
    grid_components(grid_1km, n = 0:2, type = "von_neumann"),
    c(1, 1, 1, 2, 2, 2)
  )
  expect_equal(
    grid_components(grid_1km, n = 0:2, type = "moore"),
    c(1, 1, 1, 1, 1, 1)
  )
})

test_that("stars", {
  data <- tibble::tibble(
    value = 1:5,
    value2 = 2:6,
    grid = parse_grid(c("1111", "2222", "3333", "4444", "5555"), "80km")
  ) |>
    grid_as_stars()

  expect_s3_class(data, "stars")
})

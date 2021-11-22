test_that("grid", {
  grid <- mesh_grid(X_min = c(139.80625, 139.80625),
                    Y_min = c(35.712500000000006, 35.712500000000006),
                    X_max = c(139.84375, 139.80625),
                    Y_max = c(35.72916666666667, 35.712500000000006),
                    size = "1km")
  expect_equal(vctrs::vec_size(grid[[1L]]), 12L)
  expect_equal(vctrs::vec_size(grid[[2L]]), 1L)
})

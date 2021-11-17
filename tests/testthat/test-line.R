test_that("line", {
  x <- mesh_10km(c("644142", "533946", "523667", "523503", "503033"))
  y <- c(utils::tail(x, -1), x[1])

  expect_true(is.list(mesh_line(x, y)))
  expect_true(is.list(mesh_line(list(x, y))))
})

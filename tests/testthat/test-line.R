test_that("line", {
  x <- c("644142", "533946", "523667", "523503", "503033") %>%
    as_mesh()
  y <- c(tail(x, -1), x[1])

  mesh_line(x, y)

  mesh_line(list(x, x)) %>%
    dplyr::first() %>%
    plot()
})

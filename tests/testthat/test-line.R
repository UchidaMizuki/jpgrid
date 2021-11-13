test_that("line", {
  x <- c("644142", "533946", "523667", "523503", "503033") %>%
    as_mesh()

  mesh_line(list(x)) %>%
    dplyr::first() %>%
    plot()
})

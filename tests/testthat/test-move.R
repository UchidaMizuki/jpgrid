test_that("neighbor", {
  x <- c("644142", "533946", "523667", "523503", "503033") %>%
    as_mesh()

  mesh_neighbor(x[[1]], 2) %>%
    dplyr::first() %>%
    plot()

  mesh_neighbor(x[[1]], 2,
                moore = F) %>%
    dplyr::first() %>%
    plot()
})

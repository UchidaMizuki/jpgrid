test_that("mesh_to_XY", {
  x <- c(53394526313, 5339358633, 533945764, 53394611, 523503, 5339)

  for (size in c("80km", "10km", "1km", "500m", "250m", "125m")) {
    mesh1 <- tibble::tibble(mesh = as_mesh(x,
                                           size = size))
    mesh2 <- mesh1 %>%
      dplyr::mutate(mesh_to_XY(mesh),
                    mesh = XY_to_mesh(X_center, Y_center,
                                      size = size))

    expect_equal(mesh1$mesh, mesh2$mesh)
  }
})

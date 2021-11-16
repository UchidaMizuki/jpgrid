test_that("zoom", {
  mesh_10km("533963") %>%
    mesh_zoomin(size = "10km")
})

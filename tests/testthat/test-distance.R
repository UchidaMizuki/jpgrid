test_that("distance", {
  mesh_from <- as_mesh(c("6441", "5339"))
  mesh_to <- as_mesh(c("5237", "5235"))

  distance <- mesh_distance(mesh_from, mesh_to)

  print(distance)
})

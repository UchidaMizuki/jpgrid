test_that("move", {
  mesh10km <- mesh_10km(533877)

  expect_equal(mesh_move(mesh10km, 1, 1), mesh_10km(543900))
})

test_that("neighbor", {
  mesh10km <- mesh_10km(533900)

  expect_true(setequal(as.character(mesh_neighbor(mesh10km, n = 0:1)[[1]]),
                       as.character(mesh_10km(c(533817, 533910, 533911,
                                                533807, 533900, 533901,
                                                523877, 523970, 523971)))))
})

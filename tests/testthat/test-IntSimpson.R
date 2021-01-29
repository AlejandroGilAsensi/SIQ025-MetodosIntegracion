test_that("multiplication works", {
  expect_equal(IntSimpson(cos,0,1), 1/6*(cos(0)+4*cos(1/2)+cos(1)))
})

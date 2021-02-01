test_that("multiplication works", {
  expect_equal(IntRect(cos,0,1), 1)
  expect_equal(IntRect(cos,0,1,3), 1/3*cos(0)+1/3*cos(1/3)+1/3*cos(2/3))
})

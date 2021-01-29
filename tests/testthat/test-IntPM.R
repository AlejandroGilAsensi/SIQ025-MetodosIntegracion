test_that("multiplication works", {
  expect_equal(IntPM(cos,0,1), cos(1/2))
  expect_equal(IntPM(cos,0,1,2), 1/2*cos(1/4)+1/2*cos(3/4))
})

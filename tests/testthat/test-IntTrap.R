test_that("multiplication works", {
  expect_equal(IntTrap(cos,0,1), (cos(1)+cos(0))/2)
})

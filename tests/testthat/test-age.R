test_that("overlap", {
  expect_equal(proportion_overlap(0, 5, 0, 5), 1)
  expect_equal(proportion_overlap(rep(0, 10), rep(5, 10), 0, 5), rep(1, 10))
  expect_equal(proportion_overlap(rep(0, 10), rep(5, 10), rep(0, 10), rep(5, 10)), rep(1, 10))
  
  expect_equal(proportion_overlap(0, 5, 0, 2.5), 0.5)
  expect_equal(proportion_overlap(rep(0, 10), rep(5, 10), 0, 2.5), rep(0.5, 10))
  expect_equal(proportion_overlap(rep(0, 10), rep(5, 10), rep(0, 10), rep(2.5, 10)), rep(0.5, 10))
  
  expect_equal(proportion_overlap(0, 5, 10, 10), 0)
  expect_equal(proportion_overlap(rep(0, 10), rep(5, 10), 10, 20), rep(0, 10))
  expect_equal(proportion_overlap(rep(0, 10), rep(5, 10), rep(10, 10), rep(20, 10)), rep(0, 10))
})

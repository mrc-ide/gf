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

test_that("age_specific_coverage", {
  df <- data.frame(age_lower = c(0, 5),
                   age_upper = c(5, 10),
                   smc_coverage = 0.8,
                   smc_age_lower = 0.125,
                   smc_age_upper = 5,
                   ipti_coverage = 0.8,
                   rtss_coverage = 0.8)
  
  df_adj <- age_specific_coverage(df)
  
  expect_equal(df_adj$smc_coverage, c(0.8 * (5 - 0.125) / 5, 0))
  expect_equal(df_adj$ipti_coverage, c(0.8 * (2 - 0.25) / 5, 0))
  expect_equal(df_adj$rtss_coverage, c(0.8 * (1.5 - 0.125) / 5, 0))
})
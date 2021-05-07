test_that("Year summary works", {
  df <- data.frame(year = 0:(100 * 12) / 12)
  df_target <- data.frame(year = 2000:2050)
  expect_equal(year_summary(df), df_target)
})

test_that("model_output_to_long works", {
  df <- data.frame(name1 = "run1", 
                   name2 = "part1",
                   year = 2000:2001, 
                   inc_0_10 = c(0.01, 0.02),
                   inc_10_20 = c(0.001, 0.002),
                   prev_0_10 = c(0.5, 1),
                   prev_10_20 = c(0.05, 0.1))
  df_target <- tibble::tibble(name1 = "run1", 
                          name2 = "part1",
                          year = rep(2000:2001, each = 2),
                          age_lower = c(0, 10, 0, 10),
                          age_upper = c(10, 20, 10, 20),
                          inc = c(0.01, 0.001, 0.02, 0.002),
                          prev = c(0.5, 0.05, 1, 0.1))
  expect_equal(model_output_to_long(df, name1, name2), df_target)
})

test_that("Year summary works", {
  df <- data.frame(name1 = "run1", 
                   name2 = "part1",
                   year = 0:(2 * 12) / 12, 
                   inc_0_10_smooth = 1,
                   inc_0_10 = 2,
                   inc_10_20_smooth = 3,
                   inc_10_20 = 4,
                   prev_0_10_smooth = 5,
                   prev_0_10 = 6,
                   prev_10_20_smooth = 7,
                   prev_10_20 = 8,
                   sev_0_10_smooth = 1,
                   sev_0_10 = 2,
                   sev_10_20_smooth = 3,
                   sev_10_20 = 4,
                   prop_0_10_smooth = 5,
                   prop_0_10 = 6,
                   prop_10_20_smooth = -999,
                   prop_10_20 = 8)
  df_target <- tibble::tibble(name1 = "run1", 
                      name2 = "part1",
                      year = rep(2000:2001, each = 2),
                      age_lower = c(0, 10, 0, 10),
                      age_upper = c(10, 20, 10, 20),
                      inc = c(1, 3, 1, 3),
                      prev = c(5, 7, 5, 7),
                      sev = c(1, 3, 1, 3),
                      prop = c(5, 0, 5, 0))
  expect_equal(process_raw(df, name1, name2), df_target)
})


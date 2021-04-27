test_that("Process epi works", {
  df <- data.frame(name = "test_epi",
                   age_lower = c(0, 5),
                   age_upper = c(5, 10),
                   inc = c(0.01, 0.02),
                   sev = c(0.04, 0.05),
                   prop = c(0.25, 0.75),
                   par = 1000,
                   treatment_coverage = 0.8)
  
  df <- df %>%
    par()
  expect_equal(df$par, c(0.25, 0.75) * 1000)
  
  df <- df %>%
    cases()
  expect_equal(df$cases, round(df$inc * df$par))
  
  df <- df %>%
    severe_cases()
  expect_equal(df$severe_cases, round(df$sev * df$par))
  
  df <- df %>%
    mortality_rate()
  expect_equal(df$mortality_rate, (1 - (0.5 * df$treatment_coverage)) * 0.215 * df$sev)
  
  df <- df %>%
    deaths()
  expect_equal(df$deaths, round(df$mortality_rate * df$par))
  
  df <- df %>%
    non_malarial_fevers()
  expect_equal(df$non_malarial_fevers, ifelse(df$age_upper == 5, 3.4 * df$par, df$par))
})

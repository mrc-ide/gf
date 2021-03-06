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
    outcome_uncertainty()
  expect_equal(df$cases_lower, round(pmax(0, qnorm(0.025, df$cases, df$cases * 0.227))))
  expect_equal(df$cases_upper, round(qnorm(0.975, df$cases, df$cases * 0.227)))
  expect_equal(df$deaths_lower, round(pmax(0, qnorm(0.025, df$deaths, df$deaths * 0.265))))
  expect_equal(df$deaths_upper, round(qnorm(0.975, df$deaths, df$deaths * 0.265)))
  
  df <- df %>%
    non_malarial_fevers()
  expect_equal(df$non_malarial_fevers, ifelse(df$age_upper == 5, 3.4 * df$par, df$par))
  
  df <- df %>%
    daly_components()
  expect_equal(df$yld, ifelse(df$age_upper == 5, df$cases * 0.01375 * 0.211 + df$severe_cases * 0.04795 * 0.6, 
                              df$cases * 0.01375 * 0.195 + df$severe_cases * 0.04795 * 0.6))
  expect_equal(df$yll, df$deaths * (63 - ((df$age_lower + df$age_upper) / 2)))
  
  df <- df %>%
    life_years()
  expect_equal(df$life_years, df$par - df$deaths + 0.5 * df$deaths)
})

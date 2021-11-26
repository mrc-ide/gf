test_that("commodities and services", {
  df <- data.frame(par = 1000,
                   target_use = 0.6,
                   eq_npc = 0.8,
                   optimal_eq_npc = 0.6,
                   net_type = "pyrethroid",
                   irs_coverage = 0.5,
                   irs_compound = "actellic",
                   smc_coverage = 0.8,
                   smc_rounds = 4,
                   ipti_coverage = 0.1,
                   rtss_coverage = 0.5,
                     treatment_coverage = 0.9,
                   prop_act = 0.75,
                   prop_pf = 0.75,
                   cases = 100,
                   severe_cases = 10,
                   non_malarial_fevers = 25,
                   prev = 0.6,
                   population_prevalence = 0.6,
                   hh_size = 4
                   )
  df <- commodities_and_services(df)
  
  expect_equal(df$pyrethroid_nets_distributed, round(annual_net_distibuted_gts(0.6) * 1000))
  expect_equal(df$pyrethroid_pbo_nets_distributed, 0)
  expect_equal(df$pyrethroid_chlorfenapyr_nets_distributed, 0)
  expect_equal(df$ddt_irs_people_protected, 0)
  expect_equal(df$actellic_irs_people_protected, 0.5 * 1000)
  expect_equal(df$smc_doses, 0.8 * 1000 * 4)
  expect_equal(df$ipti_doses, 0.1 * 1000 * 4)
  expect_equal(df$rtss_doses, 0.5 * 1000 * 4)
  expect_equal(df$pf_act_courses + df$pv_act_primaquine_courses + df$pf_non_act_courses, 0.9 * 100)
  expect_equal(df$pf_rdt + df$pf_microscopy + df$pv_microscopy, 0.9 * 100)
  expect_equal(df$non_malarial_fever_rdts, round(25 * 0.37 * 0.9))
  expect_equal(df$non_malarial_fever_act, round(25 * 0.37 * 0.9 * 0.6))
  expect_equal(df$inpatient_visits, round(10 * 0.9))
  expect_equal(df$outpatient_visits, round(100 * 0.9))
})


test_that("Net loss and half life", {
  expect_equal(net_loss(0), 1)
  expect_equal(net_loss(1), 0.776754)
  expect_equal(round(half_life(k = 18, l = 8.5), 2), 1.64)
})


test_that("Net distribution", {
  expect_equal(annual_net_distibuted_gts(0.8), 0.5405405, tolerance = 0.0001)
  expect_equal(annual_net_distibuted(0.8), 0.4469046, tolerance = 0.0001)
})
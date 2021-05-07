test_that("costs", {
  df <- data.frame(ISO = "AGO",
                   year = 2020,
                   age_lower = 0,
                   age_upper = 5,
                   par = 1000,
                   eq_npc = 0.8,
                   net_type = "pyrethroid",
                   irs_coverage = 0.5,
                   irs_compound = "actellic",
                   smc_coverage = 0.8,
                   smc_rounds = 4,
                   ipti_coverage = 0.1,
                   ipti_rounds = 3,
                   rtss_coverage = 0.5,
                   rtss_rounds = 4,
                   treatment_coverage = 0.9,
                   proportion_act = 0.75,
                   proportion_pf = 0.75,
                   cases = 100,
                   severe_cases = 10,
                   non_malarial_fevers = 25,
                   prev = 0.6,
                   population_prevalence = 0.6,
                   population_api = 4.5
  ) %>% 
    commodities_and_services() %>%
    tidyr::crossing(unit_costs) %>%
    dplyr::left_join(treatment_unit_costs, by = c("ISO", "age_lower", "age_upper")) %>%
    component_costs()
  
  expect_equal(df$pyrethroid_net_cost, round(df$pyrethroid_nets_distributed * unit_costs$cost_per_pyrethoid_net_delivered))
  expect_equal(df$pyrethroid_pbo_net_cost, 0)
  expect_equal(df$pyrethroid_chlorfenapyr_net_cost, 0)
  expect_equal(df$ddt_irs_cost, 0)
  expect_equal(df$actellic_irs_cost, round(df$actellic_irs_people_protected * unit_costs$cost_per_person_protected_by_actellic_irs))
  expect_equal(df$smc_cost, round(df$smc_doses * unit_costs$cost_per_smc_dose_delivered))
  expect_equal(df$ipti_cost, round(df$ipti_doses * unit_costs$cost_per_ipti_dose_delivered))
  expect_equal(df$rtss_cost, round(df$rtss_doses * unit_costs$cost_per_rtss_dose_delivered))
  expect_equal(df$rdt_cost, round(df$pf_rdt * unit_costs$cost_per_rdt))
  expect_equal(df$non_malarial_fever_rdt_cost, round(df$non_malarial_fever_rdts * unit_costs$cost_per_rdt))
  expect_equal(df$proactive_case_detection_cost, 0)
  expect_equal(df$case_investigation_cost, round(0.15 * df$cases * unit_costs$cost_per_case_investigated * (2020 - 2015) *  (1 / (2030-2015))))
  expect_equal(df$surveillance_cost, round(df$par * df$cost_per_capita_surveillance))
  
  tx <- dplyr::filter(treatment_unit_costs, ISO == df$ISO, age_lower == df$age_lower, age_upper == df$age_upper)
  expect_equal(df$act_cost, round(df$pf_act_courses * tx$cost_per_course_act))
  expect_equal(df$non_act_cost, round(df$pf_non_act_courses * tx$cost_per_course_non_act))
  expect_equal(df$act_primaquine_cost, round(df$pv_act_primaquine_courses * tx$cost_per_course_act_and_primaquine))
  expect_equal(df$inpatient_cost, round(df$inpatient_visits * tx$cost_per_inpatient_visit))
  expect_equal(df$outpatient_cost, round(df$outpatient_visits * tx$cost_per_outpatient_visit))
  
  total_cost_check <- df %>%
    dplyr::select(contains("_cost")) %>%
    sum()
  
  df <- category_costs(df)
  expect_equal(df$net_cost, df$pyrethroid_net_cost + df$pyrethroid_pbo_net_cost + df$pyrethroid_chlorfenapyr_net_cost)
  expect_equal(df$irs_cost, df$ddt_irs_cost + df$actellic_irs_cost)
  expect_equal(df$diagnostic_and_treatment_cost, df$rdt_cost + df$act_cost + df$non_act_cost + 
                 df$microscopy_cost + df$act_primaquine_cost + df$non_malarial_fever_rdt_cost + 
                 df$non_malaria_fever_act_cost + df$inpatient_cost + df$outpatient_cost)
  expect_equal(df$elimination_cost, df$case_investigation_cost + df$proactive_case_detection_cost)
  
  expect_equal(df$total_cost, total_cost_check)
})

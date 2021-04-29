component_costs <- function(x){
  x %>%
    dplyr::mutate(
      pyrethroid_net_cost = round(.data$pyrethroid_nets_distributed * .data$cost_per_pyrethoid_net_delivered),
      pyrethroid_pbo_net_cost = round(.data$pyrethroid_pbo_nets_distributed * .data$cost_per_pyrethroid_pbo_net_delivered),
      pyrethroid_chlorfenapyr_net_cost = round(.data$pyrethroid_chlorfenapyr_nets_distributed * .data$cost_per_pyrethroid_chlorfenapyr_net_delivered),
      ddt_irs_cost = round(.data$ddt_irs_people_protected * .data$cost_per_person_protected_by_ddt_irs),
      actellic_irs_cost = round(.data$actellic_irs_people_protected * .data$cost_per_person_protected_by_actellic_irs),
      smc_cost = round(.data$smc_doses * .data$cost_per_smc_dose_delivered),
      ipti_cost = round(.data$ipti_doses * .data$cost_per_ipti_dose_delivered),
      rtss_cost = round(.data$rtss_doses * .data$cost_per_rtss_dose_delivered),
      rdt_cost = round(.data$pf_rdt * .data$cost_per_rdt),
      act_cost = round(.data$pf_act_courses * .data$cost_per_course_act),
      non_act_cost = round(.data$pf_non_act_courses * .data$cost_per_course_non_act),
      microscopy_cost = round((.data$pf_microscopy + .data$pv_microscopy) * .data$cost_per_microscopy),
      act_primaquine_cost = round(.data$pv_act_primaquine_courses * .data$cost_per_course_act_and_primaquine),
      non_malarial_fever_rdt_cost = round(.data$non_malarial_fever_rdts * .data$cost_per_rdt),
      non_malaria_fever_act_cost = round(.data$non_malaria_fever_act * .data$cost_per_course_act),
      outpatient_cost = round(.data$outpatient_visits * .data$cost_per_outpatient_visit),
      inpatient_cost = round(.data$inpatient_visits * .data$cost_per_inpatient_visit),
      surveillance_cost = round(.data$par * .data$cost_per_capita_surveillance),
      case_investigation_cost = round(dplyr::case_when(.data$population_api <= 5 & .data$population_api >4 ~ 0.15 * .data$cases * .data$cost_per_case_investigated,
                                          .data$population_api <= 4 & .data$population_api >3 ~ 0.3 * .data$cases * .data$cost_per_case_investigated,
                                          .data$population_api <= 3 & .data$population_api >2 ~ 0.5 * .data$cases * .data$cost_per_case_investigated,
                                          .data$population_api <= 2 & .data$population_api >1 ~ 0.7 * .data$cases * .data$cost_per_case_investigated,
                                          .data$population_api <= 1 & .data$population_api >0.5 ~ 0.9 * .data$cases * .data$cost_per_case_investigated,
                                          .data$population_api <= 0.5 ~ 1 * .data$cases * .data$cost_per_case_investigated,
                                          TRUE ~ 0) * pmin(pmax(0, (.data$year - 2015) *  (1 / (2030-2015))), 1)),
      proactive_case_detection_cost = round(dplyr::case_when(.data$population_api <= 1 & .data$population_api >0.5 ~ 0.1 * .data$cases * .data$cost_per_proactive_case_detected,
                                                .data$population_api <= 0.05 ~ 1 * .data$cases * .data$cost_per_proactive_case_detected,
                                                TRUE ~ 0) * pmin(pmax(0, (.data$year - 2015) *  (1 / (2030-2015))), 1))
    )
}

category_costs <- function(x){
  x %>% 
    dplyr::mutate(
      net_cost = .data$pyrethroid_net_cost + .data$pyrethroid_pbo_net_cost + .data$pyrethroid_chlorfenapyr_net_cost,
      irs_cost = .data$ddt_irs_cost + .data$actellic_irs_cost,
      diagnostic_and_treatment_cost = .data$rdt_cost + .data$act_cost + .data$non_act_cost + 
        .data$microscopy_cost + .data$act_primaquine_cost + .data$non_malarial_fever_rdt_cost + 
        .data$non_malaria_fever_act_cost + .data$inpatient_cost + .data$outpatient_cost,
      elimination_cost = .data$case_investigation_cost + .data$proactive_case_detection_cost,
      total_cost = .data$net_cost + .data$irs_cost + .data$smc_cost + .data$ipti_cost + .data$rtss_cost +
        .data$diagnostic_and_treatment_cost + .data$surveillance_cost + .data$elimination_cost
    )
}
#' Select output for GF
#'
#' @param x Optimised output
#'
#' @export
gf_select <- function(x){
  x %>%
    dplyr::select(.data$ISO, .data$NAME_0, .data$NAME_1, .data$NAME_2, .data$ur,
                  .data$pre, .data$replenishment, .data$post, .data$budget_prop,
                  .data$year,
                  .data$age_lower, .data$age_upper,
                  .data$par,
                  .data$par_smc, .data$par_ipti, .data$par_rtss,
                  .data$cases, .data$cases_lower, .data$cases_upper,
                  .data$deaths, .data$deaths_lower, .data$deaths_upper,
                  .data$prev,
                  .data$life_years, .data$life_years_lower, .data$life_years_upper,
                  .data$dalys, .data$dalys_lower, .data$dalys_upper,
                  dplyr::contains("coverage"),
                  .data$llin_n, .data$irs_n,  .data$irs_hh,
                  .data$net_cost, .data$irs_cost, .data$smc_cost, .data$ipti_cost,
                  .data$rtss_cost, .data$diagnostic_and_treatment_cost,
                  .data$surveillance_cost, .data$elimination_cost, .data$total_cost) %>%
    dplyr::mutate(budget_prop = factor(.data$budget_prop))
}

#' Aggregate output
#' 
#' Aggregation can be over sites and/or age (not time).
#'
#' @param x Optimised output
#' @param ... Grouping columns
#'
#' @export
gf_aggregate <- function(x, ...){
  x %>%
    dplyr::group_by(...) %>%
    dplyr::summarise(
      cases = sum(.data$cases),
      cases_lower = sum(.data$cases_lower),
      cases_upper = sum(.data$cases_upper),
      deaths = sum(.data$deaths),
      deaths_lower = sum(.data$deaths_lower),
      deaths_upper = sum(.data$deaths_upper),
      y = sum(.data$y),
      prev = stats::weighted.mean(.data$prev, .data$par),
      life_years = sum(.data$life_years),
      life_years_lower = sum(.data$life_years_lower),
      life_years_upper = sum(.data$life_years_upper),
      dalys = sum(.data$dalys),
      dalys_lower = sum(.data$dalys_lower),
      dalys_upper = sum(.data$dalys_upper),
      treatment_coverage = stats::weighted.mean(.data$treatment_coverage, .data$par),
      net_coverage = stats::weighted.mean(.data$net_coverage, .data$par),
      net_n = sum(.data$llin_n),
      irs_coverage = stats::weighted.mean(.data$irs_coverage, .data$par),
      vector_control_coverage = stats::weighted.mean(.data$vector_control_coverage, .data$par),
      irs_people_protected = sum(.data$irs_n),
      irs_hh = sum(.data$irs_hh),
      smc_coverage = stats::weighted.mean(.data$smc_coverage, .data$par),
      rtss_coverage = stats::weighted.mean(.data$rtss_coverage, .data$par),
      ipti_coverage = stats::weighted.mean(.data$ipti_coverage, .data$par),
      iptp_coverage = stats::weighted.mean(.data$iptp_coverage, .data$par),
      iccm_coverage = stats::weighted.mean(.data$iccm_coverage, .data$par),
      diagnostic_and_treatment_cost = sum(.data$diagnostic_and_treatment_cost),
      net_cost = sum(.data$net_cost),
      irs_cost = sum(.data$irs_cost),
      smc_cost = sum(.data$smc_cost),
      rtss_cost = sum(.data$rtss_cost),
      ipti_cost = sum(.data$ipti_cost),
      surveillance_cost = sum(.data$surveillance_cost),
      elimination_cost = sum(.data$elimination_cost),
      total_cost = sum(.data$total_cost),
      par = sum(.data$par),
      par_smc = sum(.data$par_smc),
      par_ipti = sum(.data$par_ipti),
      par_rtss = sum(.data$par_rtss)
    ) %>%
    dplyr::ungroup()
}

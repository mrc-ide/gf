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
                  .data$cases, .data$deaths, .data$prev,
                  .data$life_years, .data$dalys,
                  dplyr::contains("coverage"),
                  .data$net_cost, .data$irs_cost, .data$smc_cost, .data$ipti_cost,
                  .data$rtss_cost, .data$diagnostic_and_treatment_cost,
                  .data$surveillance_cost, .data$elimination_cost, .data$total_cost) %>%
    dplyr::mutate(budget_prop = factor(budget_prop))
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
    dplyr::summarise(cases = sum(.data$cases),
              deaths = sum(.data$deaths),
              prev = stats::weighted.mean(.data$prev, .data$par),
              life_years = sum(.data$life_years),
              dalys = sum(.data$dalys),
              treatment_coverage = stats::weighted.mean(.data$treatment_coverage, .data$par),
              net_coverage = stats::weighted.mean(.data$net_coverage, .data$par),
              irs_coverage = stats::weighted.mean(.data$irs_coverage, .data$par),
              smc_coverage = stats::weighted.mean(.data$smc_coverage, .data$par),
              rtss_coverage = stats::weighted.mean(.data$rtss_coverage, .data$par),
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
              par = sum(.data$par)
    )
}

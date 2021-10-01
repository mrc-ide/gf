#' Keep only required columns - helps for memory issues with large countries
#'
#' @param x Model output
prune <- function(x){
  x %>%
    dplyr::select(.data$ISO, .data$NAME_0, .data$NAME_1, .data$NAME_2, .data$ur,
                  .data$pre, .data$replenishment, .data$post,
                  .data$year,
                  .data$age_lower, .data$age_upper,
                  .data$par,
                  .data$par_smc, .data$par_ipti, .data$par_rtss,
                  .data$cases, .data$cases_lower, .data$cases_upper,
                  .data$deaths, .data$deaths_lower, .data$deaths_upper,
                  .data$prev,
                  .data$life_years, .data$life_years_lower, .data$life_years_upper,
                  .data$yld, .data$yld_lower, .data$yld_upper,
                  .data$yll, .data$yll_lower, .data$yll_upper,
                  .data$dalys, .data$dalys_lower, .data$dalys_upper,
                  dplyr::contains("coverage"),
                  .data$net_cost, .data$irs_cost, .data$smc_cost, .data$ipti_cost,
                  .data$rtss_cost, .data$diagnostic_and_treatment_cost,
                  .data$surveillance_cost, .data$elimination_cost, .data$total_cost) 
}


#' Elimination threshold
#'
#' @param x output
#' @param threshold Incidence threshold below which outputs are fixed
elim_threshold <- function(x, threshold = -1){
  # Find if earliest year post 2020 that meets threshold criteria
  th <- x %>%
    dplyr::filter(.data$year > 2020) %>%
    dplyr::group_by(.data$year) %>%
    dplyr::summarise(inc = sum(.data$inc * .data$par) / sum(.data$par)) %>%
    dplyr::filter(.data$inc <= threshold) %>%
    dplyr::slice_min(.data$year, with_ties = FALSE)
  
  # If any threshold incidence reached, fix epi outputs and coverage inputs from that point forth
  if(nrow(th > 0)){
    cols <- c("prev", "inc", "sev", "treatment_coverage", "prop_act", "net_coverage", "net_type", "irs_coverage",
              "irs_compound", "irs_rounds", "smc_coverage", "iptp_coverage", "ipti_coverage", "iccm_coverage",
              "target_use", "eq_npc", "vector_control_coverage")
    x[x$year > th$year, cols] <- NA
    x <- x %>%
      dplyr::arrange(.data$age_lower, .data$year) %>%
      tidyr::fill(dplyr::all_of(cols), .direction = "down") %>%
      dplyr::arrange(.data$year, .data$age_lower)
  }
  return(x)
}

#' Post processing wrapper
#' 
#' Links to data, estimates epi, commodity and cost outputs
#'
#' @param model_output Model output
#' @param coverage_input Coverage input
#'
#' @export
post_process <- function(model_output, coverage_input){
  model_output %>%
    link_data(coverage_input) %>%
    any_vc_coverage() %>%
    epi_post_processing() %>%
    commodities_and_services() %>%
    component_costs() %>%
    category_costs() %>%
    prune()
}

#' Keep only required columns - helps for memory issues with large countries
#'
#' @param x model_output Model output
prune <- function(x){
  x %>%
    dplyr::select(.data$ISO, .data$NAME_0, .data$NAME_1, .data$NAME_2, .data$ur,
                .data$pre, .data$replenishment, .data$post,
                .data$year,
                .data$age_lower, .data$age_upper,
                .data$par,
                .data$cases, .data$cases_lower, .data$cases_upper,
                .data$deaths, .data$deaths_lower, .data$deaths_upper,
                .data$prev,
                .data$life_years, .data$life_years_lower, .data$life_years_upper,
                .data$dalys, .data$dalys_lower, .data$dalys_upper,
                dplyr::contains("coverage"),
                .data$net_cost, .data$irs_cost, .data$smc_cost, .data$ipti_cost,
                .data$rtss_cost, .data$diagnostic_and_treatment_cost,
                .data$surveillance_cost, .data$elimination_cost, .data$total_cost) 
}
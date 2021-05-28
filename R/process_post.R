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
    category_costs()
}
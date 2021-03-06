#' Isolate annual summary
#' 
#' To ensure compatible run options as GTS modelling we return monthly output. However,
#' to reduce storage and processing requirements we immediately trim to annual output.
#'
#' @param x Model output
#' @param baseline_year Start year
#' @param max_year End year
#'
#' @return Annual output
year_summary <- function(x, baseline_year = 2000, max_year = 2050){
  x %>%
    # We -1 here as we are using smoothed outputs (the average over the previous 12 months)
    ## Therefore the smoothed output for time 2010 is essentially the average over 2009 
    dplyr::mutate(year = round(.data$year + baseline_year - 1, 1)) %>%
    # Select the integer years in specified range
    dplyr::filter(.data$year %in% baseline_year:max_year)
}

#' Raw output to long
#' 
#' Converts wide (by age groups) to long
#'
#' @param x Model output
#' @param ... Additional columns to select (for example run names)
#'
#' @return Long output
model_output_to_long <- function(x, ...){
  x %>%
    # Convert all to long
    tidyr::pivot_longer(-c(.data$year, ...), names_to = "var", values_to = "y") %>%
    # Remove _smooth subscript for neater names
    dplyr::mutate(var = stringr::str_remove(.data$var, "_smooth")) %>%
    # Isolate lower and upper age bounds from variable names
    tidyr::separate(.data$var, into = c("type", "age_lower", "age_upper"), sep = "_", convert = TRUE) %>%
    # Convert back to wide
    tidyr::pivot_wider(id_cols = c(..., .data$year, .data$age_lower, .data$age_upper), names_from = .data$type, values_from = .data$y)
}

#' Raw output pre-processing wrapper
#'
#' @param raw_output Raw model output
#' @param threshold Incidence threshold below which outputs are fixed
#' @param input_address Address of input data
#' @param ... Additional columns to select (for example run names)
#'
#' @return Pre-processed model output
#' @export
process_raw <- function(raw_output, threshold = 0, input_address, ...){
  coverage_input <- readRDS(input_address) %>%
    dplyr::select(.data$Continent, .data$ISO, .data$NAME_0, .data$NAME_1,
                  .data$NAME_2, .data$ur, .data$pre, .data$replenishment,
                  .data$post, .data$interventions) %>%
    tidyr::unnest(col = c(.data$interventions))
  
  raw_output %>%
    # Dropping non_smooth output and intervention number output
    dplyr::select(..., .data$year, dplyr::contains("smooth")) %>%
    # Annual summary
    year_summary() %>%
    # Formatting
    model_output_to_long(...) %>%
    # Replace -999
    replace_missing() %>%
    # Process epi and coverage and costs  
    link_data(coverage_input) %>%
    any_vc_coverage() %>%
    elim_threshold(threshold = threshold) %>%
    epi_post_processing() %>%
    adjust_net_efficiency() %>%
    commodities_and_services() %>%
    component_costs() %>%
    category_costs() %>%
    prune()
}


#' Replaces -999 values in outputs
#'
#' @param x Model output
replace_missing <- function(x){
  x %>%
    dplyr::mutate(prev = ifelse(.data$prev == -999, 0, .data$prev),
                  inc = ifelse(.data$inc == -999, 0, .data$inc),
                  inc = ifelse(.data$inc == Inf, 0, .data$inc),
                  sev = ifelse(.data$sev == -999, 0, .data$sev),
                  sev = ifelse(.data$sev == Inf, 0, .data$sev),
                  prop = ifelse(.data$prop == -999, 0, .data$prop))
}

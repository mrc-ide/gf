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
    tidyr::mutate(year = round(year + baseline_year - 1, 1)) %>%
    # Select the integer years in specified range
    tidyr::filter(year %in% baseline_year:max_year)
}


#' Raw output to long
#' 
#' Converts wide (by age groups) to long
#'
#' @param x Model output
#'
#' @return Long output
model_output_to_long <- function(x, ...){
  x %>%
    # Convert all to long
    tidyr::pivot_longer(-c(year, ...), names_to = "var", values_to = "y") %>%
    # Remove _smooth subscript for neater names
    dplyr::mutate(var = str_remove(var, "_smooth")) %>%
    # Isolate lower and upper age bounds from variable names
    tidyr::separate(var, into = c("type", "lower", "upper"), sep = "_", convert = TRUE) %>%
    # Convert back to wide
    tidyr::pivot_wider(id_cols = c(..., year, lower, upper), names_from = type, values_from = y)
}


#' Raw output pre-processing wrapper
#'
#' @param raw_output Raw model output
#' @param ... Additional columns to select (for example run names)
#'
#' @return Pre-processed model output
#' @export
process_raw <- function(raw_output, ...){
  raw_output %>%
    # Dropping non_smooth output and intervention number output
    dplyr::select(..., year, dplyr::contains("smooth")) %>%
    # Annual summary
    year_summary() %>%
    # Formatiing
    model_output_to_long()
}


#' Create dataset for optimisation
#'
#' @param processed_output Processed model output
#'
#' @return Data for use in optimiation
create_optimisation_data <- function(processed_output){
  processed_output %>%
    dplyr::filter(.data$year %in% 2024:2026) %>%
    dplyr::group_by(.data$NAME_0, .data$NAME_1, .data$NAME_2, .data$ur, .data$pre, .data$replenishment) %>%
    dplyr::summarise(cases = sum(.data$cases),
                     deaths = sum(.data$deaths),
                     cost = sum(.data$total_cost)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(y = (.data$cases * (sum(.data$deaths) / sum(.data$cases))) + .data$deaths)
}
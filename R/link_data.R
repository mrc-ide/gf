#' Link simulation output with input data
#'
#' @param x Model output
#' @param coverage_input Coverage inputs for run 
#' @export
link_data <- function(x, coverage_input){
  use_npc <- get("use_npc")
  ppf <- get("ppf")
  treatment_unit_costs <- get("treatment_unit_costs")
  unit_costs <- get("unit_costs")
  population_projections <- get("population_projections")
  hh <- get("hh")
  x %>%
    # Coverage
    dplyr::left_join(coverage_input, by = c("NAME_0", "NAME_1", "NAME_2", "ur", "pre", "replenishment", "post", "year")) %>%
    add_target_use() %>%
    age_specific_coverage() %>%
    # Use to NPC
    dplyr::left_join(use_npc, by = c("ISO", "target_use")) %>%
    # Proportion pf
    dplyr::left_join(ppf, by = c("Continent", "ISO", "NAME_0", "NAME_1", "NAME_2", "ur", "year")) %>%
    # Treatment costing
    dplyr::left_join(treatment_unit_costs, by = c("ISO", "age_lower", "age_upper")) %>%
    # Other unit costs
    dplyr::bind_cols(unit_costs) %>%
    # Population
    dplyr::left_join(population_projections, by = c("Continent", "ISO", "NAME_0", "NAME_1", "NAME_2", "ur", "year")) %>%
    # HH sizes
    dplyr::left_join(hh, by = "ISO")
}
#' Create country summary report
#'
#' @param gf_optimised Optimised data for a country
#' @param gf_gp Country level annual summary from the GF runs of global plan
#' @param gts_gp Country level annual summary from the GTS global plan
#' @param global_plan_budget Replenishment period budget from the global plan
#' @param gf_fixed Fixed scenario output
#' @param comp Comparison with pre-inputs
#' @param output_file Report output address
#' @param budget_levels Budget increments used
#'
#' @export
render_report <- function(gf_optimised, gf_gp, gts_gp, global_plan_budget, gf_fixed, comp, output_file, budget_levels = paste(seq(0.05, 1, 0.05))) {
  rmd_file <- system.file("country_report.Rmd", package = "gf")
  rmarkdown::render(
    rmd_file, 
    params = list(
      gf_optimised = gf_optimised,
      gf_gp = gf_gp,
      gts_gp = gts_gp,
      global_plan_budget = global_plan_budget,
      gf_fixed = gf_fixed,
      comp = comp,
      budget_levels = budget_levels
    ),
    output_file = output_file
  )
}
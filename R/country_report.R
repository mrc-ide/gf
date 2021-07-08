#' Create country summary report
#'
#' @param gf_optimised Optimised data for a country
#' @param gts Country level annual summary from the GTS for a country
#' @param output_file Report output address
#'
#' @export
render_report <- function(gf_optimised, gts, output_file) {
  rmd_file <- system.file("country_report.Rmd", package = "gf")
  rmarkdown::render(
    rmd_file, 
    params = list(
      gf_optimised = gf_optimised,
      gts = gts
    ),
    output_file = output_file
  )
}
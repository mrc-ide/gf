#' Proportion overlap
#' 
#' Estimate the proportion overlap of \code{range} on \code{age}. Used to estimate
#' the effective coverage of an intervention whose target range does not line up
#' with output age-ranges.
#'
#' @param age_lower Lower of output age lower bound
#' @param age_upper Upper of output age upper bound
#' @param range_lower Target age lower bound
#' @param range_upper Target age upper bound
#'
#' @return Proportion(s)
proportion_overlap <- function(age_lower, age_upper, range_lower, range_upper){
  start = apply(cbind(age_lower, range_lower), 1, max)
  end = apply(cbind(age_upper, range_upper), 1, min)
  pmax(0, (end - start)) / (age_upper - age_lower)
}
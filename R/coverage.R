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

#' Age specific coverage
#' 
#' Adjust overall coverage to be age disaggregated. Where intervention target age group does not allign exactly with
#' one of more target age groups we adjust coverage as a proportion.
#'
#' @param x Model output
#' @param ipti_age_lower Lower age IPTi
#' @param ipti_age_upper Upper age for IPTi
#' @param rtss_age_lower Lower age for RTS,S
#' @param rtss_age_upper Upper age for RTS,S
age_specific_coverage <- function(x, ipti_age_lower = 0.25, ipti_age_upper = 2, rtss_age_lower = 0.125, rtss_age_upper = 1.5){
  x %>%
    dplyr::mutate(
      smc_coverage = .data$smc_coverage * proportion_overlap(.data$age_lower, .data$age_upper, .data$smc_age_lower, .data$smc_age_upper),
      ipti_coverage = .data$ipti_coverage * proportion_overlap(.data$age_lower, .data$age_upper, ipti_age_lower, ipti_age_upper),
      rtss_coverage = .data$rtss_coverage  * proportion_overlap(.data$age_lower, .data$age_upper, rtss_age_lower, rtss_age_upper)
    )
}


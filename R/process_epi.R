
#' Add age disaggregated population at risk
#'
#' @param x Model output
par <- function(x){
  x %>%
    dplyr::mutate(par = round(.data$par * .data$prop))
}

#' Add clinical cases
#'
#' @param x Model output
cases <- function(x){
  x %>%
    dplyr::mutate(cases = round(.data$inc * .data$par))
}

#' Add severe cases
#'
#' @param x Model output
severe_cases <- function(x){
  x %>%
    dplyr::mutate(severe_cases = round(.data$sev * .data$par))
}

#' Add mortality rate (dependent on severe case incidence and treatment coverage)
#' 
#' This follows methodology in \href{https://pubmed.ncbi.nlm.nih.gov/26809816/}{Griffin _et al_, 2016}.
#'
#' @param x Model output
#' @param scaler Severe case to death scaler
#' @param treatment_scaler Treatment modifier
mortality_rate <- function(x, scaler = 0.215, treatment_scaler = 0.5){
  x %>%
    dplyr::mutate(mortality_rate = (1 - (treatment_scaler * .data$treatment_coverage)) * scaler * .data$sev)
}

#' Add deaths
#'
#' @param x Model output
deaths <- function(x){
  x %>%
    dplyr::mutate(deaths = round(.data$mortality_rate * .data$par))
}

#' Add non-malarial fevers (NMFs).
#' 
#' Assume a constant rate of NMFs in under 5s and over 5s. This follows methodology
#' in \href{https://gh.bmj.com/content/2/2/e000176}{Patouillard _et al_, 2017}.
#'
#' @param x Model output
#' @param rate_under_5 Annual incidence of NMFs in children under 5
#' @param rate_over_5 Annual incidence of NMFs in individuals over 5
non_malarial_fevers <- function(x, rate_under_5 = 3.4, rate_over_5 = 1){
  x %>%
    dplyr::mutate(non_malarial_fevers = round(ifelse(.data$upper == 5, rate_under_5 * .data$par, rate_over_5 * .data$par)))
}
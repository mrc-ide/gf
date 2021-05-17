#' Commodity and service estimates
#' 
#' We use the net model (see net-specific functions) for the number of LLINs.
#' We approximate microscopy->act transition using the proportion acts.
#' We assume, that for areas with significant ongoing transmission (prev > 5%), 37% of NMFs are tested with RDTs (assumming proprtion_act = 1) - this is informed from the DHS.
#' We further assume, that of those NMFs tested with RDTS and prevalence-dependent proportion return a +ve rdt and are treated.
#' 
#'
#' @param x Model output
commodities_and_services <- function(x){
  x %>%
    dplyr::mutate(
      eq_npc = ifelse(.data$target_use == 0, 0, eq_npc),
      pyrethroid_nets_distributed = ifelse(.data$net_type == "pyrethroid", round(annual_net_distibuted_gts(.data$eq_npc) * .data$par), 0),
      pyrethroid_pbo_nets_distributed = ifelse(.data$net_type == "pbo", round(annual_net_distibuted_gts(.data$eq_npc) * .data$par), 0),
      pyrethroid_chlorfenapyr_nets_distributed = ifelse(.data$net_type == "ig2", round(annual_net_distibuted_gts(.data$eq_npc) * .data$par), 0),
      ddt_irs_people_protected = ifelse(.data$irs_compound == "ddt", round(.data$irs_coverage * .data$par), 0),
      actellic_irs_people_protected = ifelse(.data$irs_compound == "actellic", round(.data$irs_coverage * .data$par), 0),
      smc_doses = round(.data$smc_coverage * .data$par * .data$smc_rounds),
      ipti_doses = round(.data$ipti_coverage * .data$par * 4),
      rtss_doses = round(.data$rtss_coverage * .data$par * 4),
      pf_act_courses = round(.data$treatment_coverage * .data$prop_act * .data$cases * .data$prop_pf),
      pf_non_act_courses = round(.data$treatment_coverage * (1 - .data$prop_act) * .data$cases * .data$prop_pf),
      pf_rdt = round(.data$treatment_coverage * .data$prop_act * .data$cases * .data$prop_pf),
      pf_microscopy = round(.data$treatment_coverage * (1 - .data$prop_act) * .data$cases * .data$prop_pf),
      pv_act_primaquine_courses = round(.data$treatment_coverage * .data$cases * (1 - .data$prop_pf)),
      pv_microscopy =  round(.data$treatment_coverage * .data$cases * (1 - .data$prop_pf)),
      non_malarial_fever_rdts = round(ifelse(.data$population_prevalence > 0.05, 0.37 * .data$non_malarial_fevers * .data$treatment_coverage, 0)),
      non_malarial_fever_act = round(.data$non_malarial_fever_rdts * .data$prev),
      outpatient_visits = round(.data$treatment_coverage * .data$cases),
      inpatient_visits = round(.data$treatment_coverage * .data$severe_cases)
    )
}

#' Annual nets distributed
#' 
#' The annual number of nets distributed to maintain the input equilibrium nets per capita. 
#' This is for the specific "smoothed" case where we assume that 1/3 of the population at risk
#' is distributed nets every 3 years (this is equivalent to a smoothed 3 yearly whole population distribution).
#' This version is a simplified approximation with 3 values for proportion of nets remaining (1, 2, 3 years) scraped from 
#' \href{https://elifesciences.org/articles/09672#app1}{Bhatt _et al_, 2015}. We use this approach for consistency with GTS.
#' For a full approach with more updated fits see \code{\link{annual_net_distibuted}}. This newer approach is slightly more
#' optimistic than this approach (although overall uncertainty in these projections is large).
#'
#' @param eq_npc Equilibrium nets per capita
annual_net_distibuted_gts <- function(eq_npc){
  eq_npc / (0.88 + 0.48 + 0.12)
}

#' Net loss function
#' 
#' Adapted from:
#' 1. \href{https://elifesciences.org/articles/09672#app1}{Bhatt _et al_, 2015}
#' 2. \href{https://www.researchsquare.com/article/rs-199628/v1}{Bertozzi-Villa _et al_, 2021 (Under review)}
#'
#' @param t Time (years)
#' @param k Rate (fixed from paper)
#' @param l Time at which all nets = 0 (l is estimated so that \code{\link{half_life}}  = 1.64 years (median retention span across all countries))
#'
#' @return Proportion of nets retained
net_loss <- function(t, k = 18, l = 8.5) {
  v <- exp(k - k / (1 - (t / l) ^ 2))
  v[t >= l] <- 0
  return(v)
}

#' Half life net loss function
#'
#' @inheritParams net_loss
half_life <- function(k, l) {
  l * sqrt(1 - k / (k - log(0.5)))
}

#' Annual nets distributed
#' 
#' The annual number of nets distributed to maintain the input equilibrium nets per capita. 
#' This is for the specific "smoothed" case where we assume that 1/3 of the population at risk
#' is distributed nets every 3 years (this is equivalent to a smoothed 3 yearly whole population distribution).
#'
#' @param eq_npc Equilibrium nets per capita
annual_net_distibuted <- function(eq_npc){
  eq_npc * stats::integrate(net_loss, lower = 0, upper = 3)$value * (1/3)
}

#' Find nearest LLIN target use to match to NPC outputs
#'
#' @param x Model output
add_target_use <- function(x){
  x %>%
    dplyr::mutate(target_use = ifelse(net_coverage < 0.05 & net_coverage > 0, 0.1, round(net_coverage, 1)))
}
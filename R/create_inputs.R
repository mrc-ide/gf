#' Rename interventions
#'
#' @param x GP interventions data.frame
#' @export
interventions_rename <- function(x){
  x %>%
    dplyr::rename(treatment_coverage = .data$tx,
                  net_coverage = .data$llin,
                  irs_coverage = .data$irs,
                  smc_coverage = .data$smc,
                  rtss_coverage = .data$rtss,
                  iptp_coverage = .data$iptp,
                  ipti_coverage = .data$ipti)
}

#' Extend any inputs that do not span the full modelled time horizon
#'
#' @param x Input data.frame
#' @export
extend <- function(x){
  x %>% 
    tidyr::complete(year = 2000:2051) %>%
    tidyr::fill(-.data$year, .direction = "down")
}

#' Split out iccm coverage from treatment coverage
#'
#' @param x Interventions data.frame
#' @export
back_adjust_iccm <- function(x){
  x %>%
    dplyr::mutate(iccm_coverage = .data$iccm * 0.1,
           treatment_coverage  = pmax(0, .data$treatment_coverage - .data$iccm_coverage))
}

#' Combine iccm coverage with treatment coverage
#'
#' @param x Interventions data.frame
forward_adjust_iccm <- function(x){
  x %>%
    dplyr::mutate(treatment_coverage = pmin(1, .data$treatment_coverage + .data$iccm_coverage),
           iccm = ifelse(.data$iccm_coverage > 0, 1, 0))
}

#' Create set of binary coverage options
#' 
#' Return 0 if no coverage in GP reference year or 0, 1 if there was,
#'
#' @param reference_coverage GP coverage in 2026
#'
#' @export
coverage_options <- function(reference_coverage){
  if(reference_coverage > 0){
    c(0, 1)
  } else {
    0
  }
}

#' Create matrix of coverage options
#' 
#' Binary indcator if intervention is to be implemented (at GP coverage) or not
#'
#' @param ref_year Interventions for the GP year 2026
create_intervention_option_matrix <- function(ref_year){
  sub_coverage_options <- expand.grid(tx = coverage_options(ref_year$treatment_coverage),
                                      llin = coverage_options(ref_year$net_coverage),
                                      irs = coverage_options(ref_year$irs_coverage),
                                      smc = coverage_options(ref_year$smc_coverage),
                                      rtss = coverage_options(ref_year$rtss_coverage),
                                      iccm = coverage_options(ref_year$iccm_coverage),
                                      ipti = coverage_options(ref_year$ipti_coverage)) %>%
    # Remove the last row which should == the GP
    dplyr::slice(1:(dplyr::n() - 1))
  return(sub_coverage_options)
}

#' Modify GP intervention data.frame for coverage options
#'
#' @param tx Binary indicator for treatment
#' @param llin Binary indicator for nets 
#' @param irs  Binary indicator for irs
#' @param smc  Binary indicator for smc
#' @param rtss  Binary indicator for rtss
#' @param iccm  Binary indicator for iccm
#' @param ipti  Binary indicator for ipti
#' @param gp_interventions intervention data.frame from the GP
create_coverage <- function(tx, llin, irs, smc, rtss, iccm, ipti, gp_interventions){
  if(tx == 0){
    gp_interventions[gp_interventions$year %in% c(2024:2026), "treatment_coverage"] <- 0
  }
  if(llin == 0){
    gp_interventions[gp_interventions$year %in% c(2024:2026), "net_coverage"] <- 0
  }
  if(irs == 0){
    gp_interventions[gp_interventions$year %in% c(2024:2026), "irs_coverage"] <- 0
  }
  if(smc == 0){
    gp_interventions[gp_interventions$year %in% c(2024:2026), "smc_coverage"] <- 0
  }
  if(rtss == 0){
    gp_interventions[gp_interventions$year %in% c(2024:2026), "rtss_coverage"] <- 0
  }
  if(iccm == 0){
    gp_interventions[gp_interventions$year %in% c(2024:2026), "iccm_coverage"] <- 0
  }
  if(ipti == 0){
    gp_interventions[gp_interventions$year %in% c(2024:2026), "ipti_coverage"] <- 0
  }
  gp_interventions
}

#' Create the replenishment naming scheme indicating which interventions are on and off during the replenishment period
#'
#' @param intervention_binary_options Binary indicator if intervention is on or off
#' @param names Intervention names
create_names <- function(intervention_binary_options, names){
  out <- paste(names[intervention_binary_options == 1], collapse = "_")
  if(out == ""){
    out <- "none"
  }
  return(out)
}

#' Make replenishment options
#'
#' @param gp_input_single A input row (site) from the GP
#'
#' @return data.frame of all options for the site
#' @export
replenishment_options <- function(gp_input_single){
  # Create all possible combinations of interventions given interventions available in the GP in 2026
  intervention_option_matrix <- create_intervention_option_matrix(dplyr::filter(gp_input_single$interventions[[1]], .data$year == 2026))
  # Create modified intervention inputs for each set of intervention options
  intervention_options <- c(purrr::pmap(intervention_option_matrix, create_coverage, gp_interventions = gp_input_single$interventions[[1]]), gp_input_single$interventions)
  # Create a unique name for each set of modified intervention inputs
  replenishment_names <- c(apply(intervention_option_matrix, 1, create_names, names = names(intervention_option_matrix)), "gp")
  # Create the full set of inputs for each set of modified intervention inputs
  output <- gp_input_single[rep(1, length(intervention_options)),] %>%
    dplyr::mutate(pre = "gp",
                  post = "gp",
                  replenishment = replenishment_names,
                  interventions = intervention_options,
                  interventions = lapply(.data$interventions, forward_adjust_iccm)) %>%
    dplyr::select(.data$Continent, .data$ISO, .data$NAME_0, .data$NAME_1, .data$NAME_2,.data$ur, .data$pre, .data$replenishment, .data$post, .data$interventions, tidyr::everything())
  return(output)
}
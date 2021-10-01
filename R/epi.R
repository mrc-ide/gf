#' Epidemiological post processing
#'
#' @param x Model output
epi_post_processing <- function(x){
  x %>%
    par() %>%
    cases() %>%
    severe_cases() %>%
    mortality_rate() %>%
    deaths() %>%
    outcome_uncertainty() %>%
    non_malarial_fevers() %>%
    population_indicators() %>%
    daly_components(lifespan = 63) %>%
    dalys_cast_forward(lifespan = 63, .data$Continent, .data$ISO, .data$NAME_0,
                       .data$NAME_1, .data$NAME_2, .data$ur, .data$pre,
                       .data$replenishment, .data$post, .data$age_lower,
                       .data$age_upper) %>%
    life_years()
}

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
    dplyr::mutate(non_malarial_fevers = round(ifelse(.data$age_upper == 5, rate_under_5 * .data$par, rate_over_5 * .data$par)))
}


#' Add DALY components
#'
#' Weights from \href{https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4772264/}{Gunda _et al_, 2016}
#' 
#' @param x Model output
#' @param lifespan Average life expectancy
#' @param episode_length Average length of clinical episode
#' @param severe_episode_length Average length of severe episode
#' @param weight1 Disability weight age group 1
#' @param weight2 Disability weight age group 2
#' @param weight3 Disability weight age group 3
#' @param severe_weight Disability weight severe malaria
daly_components <- function(x, lifespan = 63, 
                            episode_length = 0.01375, severe_episode_length = 0.04795,
                            weight1 = 0.211, weight2 = 0.195, weight3 = 0.172,
                            severe_weight = 0.6){
  x %>%
    dplyr::mutate(yll = .data$deaths * (lifespan - ((.data$age_lower + .data$age_upper) / 2)),
                  yll_lower = .data$deaths_lower * (lifespan - ((.data$age_lower + .data$age_upper) / 2)),
                  yll_upper = .data$deaths_upper * (lifespan - ((.data$age_lower + .data$age_upper) / 2)),
                  yld = dplyr::case_when(.data$age_upper <= 5 ~ .data$cases * episode_length * weight1 + .data$severe_cases * severe_episode_length * severe_weight,
                                         .data$age_upper > 5 & .data$age_upper <= 15 ~ .data$cases * episode_length * weight2 + .data$severe_cases * severe_episode_length * severe_weight,
                                         .data$age_upper > 15 ~ .data$cases * episode_length * weight3 + .data$severe_cases * severe_episode_length * severe_weight),
                  yld_lower = dplyr::case_when(.data$age_upper <= 5 ~ .data$cases_lower * episode_length * weight1 + .data$severe_cases * severe_episode_length * severe_weight,
                                         .data$age_upper > 5 & .data$age_upper <= 15 ~ .data$cases_lower * episode_length * weight2 + .data$severe_cases * severe_episode_length * severe_weight,
                                         .data$age_upper > 15 ~ .data$cases_lower * episode_length * weight3 + .data$severe_cases * severe_episode_length * severe_weight),
                  yld_upper = dplyr::case_when(.data$age_upper <= 5 ~ .data$cases_upper * episode_length * weight1 + .data$severe_cases * severe_episode_length * severe_weight,
                                         .data$age_upper > 5 & .data$age_upper <= 15 ~ .data$cases_upper * episode_length * weight2 + .data$severe_cases * severe_episode_length * severe_weight,
                                         .data$age_upper > 15 ~ .data$cases_upper * episode_length * weight3 + .data$severe_cases * severe_episode_length * severe_weight))
}

#' Add Life years lived
#' 
#' Years of life lived in year Y for age group A (assuming deaths occur on average half way through the year)
#'
#' @param x Model output
life_years <- function(x){
  x %>%
    dplyr::mutate(life_years = 1 * (.data$par - .data$deaths) + 0.5 * .data$deaths,
                  life_years_upper = 1 * (.data$par - .data$deaths_lower) + 0.5 * .data$deaths_lower,
                  life_years_lower = 1 * (.data$par - .data$deaths_upper) + 0.5 * .data$deaths_upper)
}


#' Sum of deaths in last "lifetime left" years
#'
#' @param row Row index
#' @param lifeleft Years of life left
#' @param cumulative_deaths Cumulative deaths
yll_cast_forward <- function(row, lifeleft, cumulative_deaths){
  if(lifeleft < 0) {
    return(0)
  } else{
    return(cumulative_deaths[row] - ifelse(row - lifeleft <= 1, pmax(0, row - lifeleft), cumulative_deaths[row - lifeleft]))
  }
}

#' Dalys case forward
#' 
#' Where, for example, a death of newborn in year 2000 with a life expectancy of 63 will add 1 YLL for each year from 2000:2062
#'
#' @param x Model output
#' @param lifespan Average life expectancy
#' @param ... Grouping arguments
dalys_cast_forward <- function(x, lifespan, ...){
  x %>%
    dplyr::group_by(...) %>%
    dplyr::arrange(.data$year, .by_group = TRUE) %>%
    dplyr::mutate(csd = cumsum(.data$deaths),
                  yll_cast_forwards = purrr::map2_dbl(dplyr::row_number(),  (lifespan - ((.data$age_upper + .data$age_lower) / 2)), yll_cast_forward, cumulative_deaths = .data$csd),
                  csd_lower = cumsum(.data$deaths_lower),
                  yll_cast_forwards_lower = purrr::map2_dbl(dplyr::row_number(),  (lifespan - ((.data$age_upper + .data$age_lower) / 2)), yll_cast_forward, cumulative_deaths = .data$csd_lower),
                  csd_upper = cumsum(.data$deaths_upper),
                  yll_cast_forwards_upper = purrr::map2_dbl(dplyr::row_number(),  (lifespan - ((.data$age_upper + .data$age_lower) / 2)), yll_cast_forward, cumulative_deaths = .data$csd_upper)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(dalys = .data$yld + .data$yll_cast_forwards,
                  dalys_lower= .data$yld_lower + .data$yll_cast_forwards_lower,
                  dalys_upper = .data$yld_upper + .data$yll_cast_forwards_upper) %>%
    dplyr::select(-.data$csd, -.data$csd_lower, -.data$csd_upper, -.data$yll, -.data$yll_lower, -.data$yll_upper) %>%
    dplyr::rename(yll = .data$yll_cast_forwards,
                  yll_lower = .data$yll_cast_forwards_lower,
                  yll_upper = .data$yll_cast_forwards_upper)
}

#' Add some population-level indicators
#' 
#' These indicators aggregate prevalence and API summaries across age groups to inform
#' some strategy thresholds in costing. These are not summaries at the country level as
#' this would require re-running for each iteration of the optimisation, therefore they
#' are all pre-computed at the site level
#'
#' @param x Model output
population_indicators <- function(x){
  x %>%
    dplyr::group_by(.data$Continent, .data$ISO, .data$NAME_0, .data$NAME_1, .data$NAME_2, .data$ur, .data$pre, .data$replenishment, .data$post, .data$year) %>%
    dplyr::mutate(population_prevalence = ifelse(sum(.data$par) == 0, 0, stats::weighted.mean(.data$prev, .data$par)),
                  population_api = ifelse(sum(.data$par) ==0, 0, (sum(.data$cases) / sum(.data$par)) * 365)) %>%
    dplyr::ungroup()
}

#' Add case and death uncertainty
#'
#' @param x  Model output
#' @param cases_cv Case uncertainty SD scaler
#' @param deaths_cv Death uncertainty SD scaler
outcome_uncertainty <- function(x, cases_cv = 0.227, deaths_cv = 0.265){
  x %>%
    dplyr::mutate(cases_lower = round(pmax(0, stats::qnorm(0.025, .data$cases, .data$cases * cases_cv))),
                  cases_upper = round(stats::qnorm(0.975, .data$cases, .data$cases * cases_cv)),
                  deaths_lower = round(pmax(0, stats::qnorm(0.025, .data$deaths, .data$deaths * deaths_cv))),
                  deaths_upper = round(stats::qnorm(0.975, .data$deaths, .data$deaths * deaths_cv)))
}

# Assume this is for a single instance of the lowest level site:
elimination_epi_threshold_adjustment <- function(x, threshold = x){
  
  
}
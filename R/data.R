#' Unit costs
#'
#' A dataset containing the non-country specific (non-treatment) unit costs. 
#' These were sources from a combination of costing from the original GTS
#'  \href{https://gh.bmj.com/content/2/2/e000176}{Patouillard _et al_, 2017} and an updated costing review (Ehler _et al_, in prep).
"unit_costs"

#' Treatment unit costs
#'
#' A dataset containing the age- and country- specific (treatment) unit costs. 
#' These were sources from a combination of costing from the original GTS
#'  \href{https://gh.bmj.com/content/2/2/e000176}{Patouillard _et al_, 2017} and an updated costing review (Ehler _et al_, in prep).
#'  and the WHOCHOICE data base \href{https://www.who.int/choice/cost-effectiveness/en/}{WHO CHOICE}.
"treatment_unit_costs"

#' Proportion pf
#' 
#' Proportion of cases that are _plasmodium falciparum_. Sourced from the WHO GMP. Proportion is assumed constant in the future.
"ppf"

#' Population projections
#' 
#' Population and population at risk projections.
#' \href{https://ghsl.jrc.ec.europa.eu/index.php}{European Commission. GHSL - Global Human Settlement Layer}
#' Lloyd _et al_. Data Descriptor: High resolution global gridded data for use in population studies. Nat Sci Data. 2017
#' \href{https://population.un.org/wpp/}{United Nations. World Population Prospects. 2019}
#' \href{https://population.un.org/wup/ }{United Nations. World Urbanization Prospects. 2018.}
"population_projections"

#' List of countries to model for the GF
#' 
"gf_countries"

#' Use to npc
#' 
"use_npc"

#' WHO burden estimates (callibrated to)
#' 
"who_burden"

#' Average HH size
#' 
"hh"

#' Case weighting.
#' 
#' Ratio of deaths:cases from GP to use to downscale cases to weight them 
#' equally to deaths in optimisation.
#' 
"case_weighting"



commodities <- function(x){
  x %>%
    mutate(llins_nets_distirbuted = ,
           irs_people_protected = irs_coverage * par,
           smc_doses = smc_coverage * par * smc_rounds,
           ipti_doses = ipti_coverage * par * ipti_rounds,
           rtss_doses = rtss_coverage * par * rtss_rounds)
}
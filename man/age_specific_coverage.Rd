% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coverage.R
\name{age_specific_coverage}
\alias{age_specific_coverage}
\title{Age specific coverage}
\usage{
age_specific_coverage(
  x,
  smc_age_lower = 0.5,
  smc_age_upper = 5,
  ipti_age_lower = 0.25,
  ipti_age_upper = 2,
  rtss_age_lower = 0.125,
  rtss_age_upper = 1.5
)
}
\arguments{
\item{x}{Model output}

\item{smc_age_lower}{Lower age for SMC}

\item{smc_age_upper}{Upper age for SMC}

\item{ipti_age_lower}{Lower age IPTi}

\item{ipti_age_upper}{Upper age for IPTi}

\item{rtss_age_lower}{Lower age for RTS,S}

\item{rtss_age_upper}{Upper age for RTS,S}
}
\description{
Adjust overall coverage to be age disaggregated. Where intervention target age group does not align exactly with
one of more target age groups we adjust coverage as a proportion.
}

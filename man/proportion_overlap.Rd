% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coverage.R
\name{proportion_overlap}
\alias{proportion_overlap}
\title{Proportion overlap}
\usage{
proportion_overlap(age_lower, age_upper, range_lower, range_upper)
}
\arguments{
\item{age_lower}{Lower of output age lower bound}

\item{age_upper}{Upper of output age upper bound}

\item{range_lower}{Target age lower bound}

\item{range_upper}{Target age upper bound}
}
\value{
Proportion(s)
}
\description{
Estimate the proportion overlap of \code{range} on \code{age}. Used to estimate
the effective coverage of an intervention whose target range does not line up
with output age-ranges.
}

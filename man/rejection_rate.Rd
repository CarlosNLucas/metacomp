% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metrics.R
\name{rejection_rate}
\alias{rejection_rate}
\title{Calculation of rejection rate for a list of estimated effects}
\usage{
rejection_rate(results, population_effect)
}
\arguments{
\item{results}{List of estimated effects bounds in format
( lower bound, upper bound )}

\item{population_effect}{Effect of the population}
}
\value{
Rate of rejection, in range 0-1
}
\description{
Calculation of rejection rate for a list of estimated effects
}
\examples{
rejection_rate(list(list(c(1,3), c(-1, 4), c(1,4)), 3.2)
}

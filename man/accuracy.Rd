% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metrics.R
\name{accuracy}
\alias{accuracy}
\title{Calculation of accuracy, as in the empirical probability of the
meta-analysis method confidence interval (1 − α) containing the
population value δ}
\usage{
accuracy(results, population_effect)
}
\arguments{
\item{results}{List of estimated effects bounds in format
( lower bound, upper bound )}

\item{population_effect}{Effect of the population}
}
\value{
Accuracy, in range 0-1
}
\description{
Calculation of accuracy, as in the empirical probability of the
meta-analysis method confidence interval (1 − α) containing the
population value δ
}
\examples{
accuracy(list(c(1,3), c(-1, 4), c(1,4)), 3.2)
}

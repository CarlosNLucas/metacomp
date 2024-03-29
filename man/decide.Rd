% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compare.R
\name{decide}
\alias{decide}
\title{Compare two methods and find points of equivalence}
\usage{
decide(
  methods,
  metric,
  min_n_experiments,
  max_n_experiments,
  min_sample_size,
  max_sample_size,
  mean_e,
  mean_c,
  sd_e,
  sd_c,
  distribution = "normal",
  parameter_of_interest = ""
)
}
\arguments{
\item{methods}{list of methods}

\item{metric}{metric to evaluate}

\item{min_n_experiments}{minimum number of experiments}

\item{max_n_experiments}{maximum number of experiments}

\item{min_sample_size}{minimum number of sample size}

\item{max_sample_size}{maximum number of sample size}

\item{mean_e}{mean in experimental group}

\item{mean_c}{mean in control group}

\item{sd_e}{Standard deviation in experimental group}

\item{sd_c}{dard deviation in control group}

\item{distribution}{Distibution model}

\item{parameter_of_interest}{parameter of interest}
}
\value{
points of equivalence
}
\description{
Compare two methods and find points of equivalence
}

#' Estimated treatment effect and
#' lower and upper confidence interval limits
#' using Hedges method (Hedges & Olkin, 1985),
#' implementation from package Meta.
#'
#' For one experiment or study the individual
#' effect is calculated, for more than one
#' the overall effect is calculated
#'
#' @param sample_size Number of observations
#' @param mean_e Estimated mean in experimental group
#' @param mean_c Estimated mean in control group
#' @param sd_e Standard deviation in experimental group
#' @param sd_c Standard deviation in control group
#' @param n_experiments Number of experiments
#' @param distribution Distibution model
#' @param return Return mode
#'
#' @return Bounds, population effect or sample size based
#' on parameter return
#' @export
#'
#' @examples
#' hedges_meta(10, 10, 3, 8, 3, 12, "population effect")
hedges_meta <- function(sample_size,
                        mean_e,
                        sd_e,
                        mean_c,
                        sd_c,
                        n_experiments,
                        distribution = "normal",
                        return = NULL) {
  e <- list()
  c <- list()

  samples <- sample_generator(n_experiments,
                              sample_size,
                              mean_e,
                              sd_e,
                              mean_c,
                              sd_c,
                              distribution)
  e <- samples[[1]]
  c <- samples[[2]]

  overall_sample_size <- rep(sample_size, n_experiments)
  overall_mean_e <- sapply(e, mean)
  overall_mean_c <- sapply(c, mean)
  overall_sd_e   <- sapply(e, sd)
  overall_sd_c   <- sapply(c, sd)

  d <- meta::metacont(overall_sample_size,
                      overall_mean_e,
                      overall_sd_e,
                      overall_sample_size,
                      overall_mean_c,
                      overall_sd_c,
                      sm = "SMD",
                      random = FALSE)

  if (return == "limits") {
    result <- list(d$lower.fixed, d$upper.fixed)
  }

  if (return == "population effect") {
    result <- (mean_e - mean_c) / ((sd_e + sd_c) / 2)
  }

  if (return == "sample effect") {
    result <- d$TE.fixed
  }

  result

}


#' Estimated treatment effect and
#' lower and upper confidence interval limits
#' using the Parametric Response Ratio method
#' (Hedges et al, 1999), also known as
#' Ratio of Means,
#' implementation from package Meta.
#'
#' For one experiment or study the individual
#' effect is calculated, for more than one
#' the overall effect is calculated
#'
#' @param sample_size Number of observations
#' @param mean_e Estimated mean in experimental group
#' @param mean_c Estimated mean in control group
#' @param sd_e Standard deviation in experimental group
#' @param sd_c Standard deviation in control group
#' @param n_experiments Number of experiments
#' @param distribution Distibution model
#' @param return Return mode
#'
#' @return Bounds, population effect or sample size based
#' on parameter return
#' @export
#'
#' @examples
#' hedges_meta(10, 10, 3, 8, 3, 12, "population effect")
parametric_rr_meta <- function(sample_size,
                               mean_e,
                               sd_e,
                               mean_c,
                               sd_c,
                               n_experiments,
                               distribution = "normal",
                               return = NULL) {

  e <- list()
  c <- list()

  samples <- sample_generator(n_experiments,
                              sample_size,
                              mean_e,
                              sd_e,
                              mean_c,
                              sd_c,
                              distribution)
  e <- samples[[1]]
  c <- samples[[2]]


  overall_sample_size <- rep(sample_size, n_experiments)
  # TODO: Volver a método antiguo de bucle
  overall_mean_e <- sapply(e, mean)
  overall_mean_c <- sapply(c, mean)
  overall_sd_e   <- sapply(e, sd)
  overall_sd_c   <- sapply(c, sd)

  rr <- meta::metacont(overall_sample_size,
                       overall_mean_e,
                       overall_sd_e,
                       overall_sample_size,
                       overall_mean_c,
                       overall_sd_c,
                       sm = "ROM",
                       random = FALSE,
                       backtransf = TRUE)

  if (return == "limits") {
    result <- list(rr$lower.fixed, rr$upper.fixed)
  }

  if (return == "population effect") {
    result <- log(mean_e / mean_c)
  }

  if (return == "sample effect") {
    result <- rr$TE.fixed
  }

  result
}


#' lower and upper confidence interval limits

#'
#' @param n_experiments Number of experiments
#' @param sample_size Number of observations
#' @param mean_e Estimated mean in experimental group
#' @param sd_e Standard deviation in experimental group
#' @param mean_c Estimated mean in control group
#' @param sd_c Standard deviation in control group
#' @param distribution Distibution model

#' @return List of generated samples for
#' experimental and control group
#' @export
#'
#' @examples
#' sample_generator(12, 50, 6, 3, 5, 3, "normal")
sample_generator <- function(n_experiments,
                             sample_size,
                             mean_e,
                             sd_e,
                             mean_c,
                             sd_c,
                             distribution) {

  e <- list()
  c <- list()

  if (distribution == "normal") {
    for (i in 1:n_experiments) {
      e <- c(e, list(stats::rnorm(sample_size, mean_e, sd_e)))
      c <- c(c, list(stats::rnorm(sample_size, mean_c, sd_e)))
    }
  } else if (distribution == "lognormal") {
    for (i in 1:n_experiments) {
      e <- c(e, list(stats::rlnorm(sample_size, mean_e, sd_e)))
      c <- c(c, list(stats::rlnorm(sample_size, mean_c, sd_e)))
    }
  } else if (distribution == "truncated") {
    for (i in 1:n_experiments) {
      e <- c(e, list(EnvStats::rnormTrunc(sample_size, mean_e, sd_e, min = 0)))
      c <- c(c, list(EnvStats::rnormTrunc(sample_size, mean_e, sd_e, min = 0)))
    }
  }

  list(e, c)
}

# TODO: probar casos extremos para generar tests necesarios

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
#' @param sd_e Standard deviation
#' @param n_experiments Number of experiments
#'
#' @return List of effect size, lower and upper intervals
#' @export
#'
#' @examples
#' hedges_meta(10, 6, 5, 3, 3)
hedges_meta <- function(sample_size,
                        mean_e,
                        mean_c,
                        sd_e,
                        n_experiments,
                        distribution = "normal"){
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
                      fixed = TRUE)

  result <- list(d$TE.fixed, d$lower.fixed, d$upper.fixed)
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
#' @param sd_e Standard deviation
#' @param n_experiments Number of experiments
#'
#' @return List of effect size, lower and upper intervals
#' @export
#'
#' @examples
#' parametric_rr_meta(10, 6, 5, 3, 3)
parametric_rr_meta <- function(sample_size,
                               mean_e,
                               mean_c,
                               sd_e,
                               n_experiments,
                               distribution = "normal"){

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

  d <- meta::metacont(overall_sample_size,
                      overall_mean_e,
                      overall_sd_e,
                      overall_sample_size,
                      overall_mean_c,
                      overall_sd_c,
                      sm = "ROM",
                      fixed = TRUE)

  result <- list(d$TE.fixed, d$lower.fixed, d$upper.fixed)

}

cliffs_delta <- function(sample_size,
                         mean_e,
                         mean_c,
                         n_experiments){

  # effsize
}

sample_generator <- function(n_experiments,
                             sample_size,
                             mean_e,
                             sd_e,
                             mean_c,
                             sd_c,
                             distribution){

  e <- list()
  c <- list()

  # if (distribution == "normal") {
  #   dist_func <- stats::rnorm()
  # } else if (distribution == "lognormal") {
  #   dist_func <- stats::rlnorm()
  # } else if (distribution == "truncated") {
  #   dist_func <- envstats::rnormTrunc()
  # }
  # do.call( ... )

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

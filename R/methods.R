# probar casos extremos para generar tests necesarios

#' Estimated treatment effect and
#' lower and upper confidence interval limits
#' using Hedges method [Hedges, L.; Olkin, I.; 1985]:
#'
#' d = J(N-2)(Y_e - Y_c)/S_p
#'
#' where
#' d is the effect size
#' J (N – 2) = is the correction factor
#' Y is the mean of experimental (E) and control (C)
#' groups
#' Sp is the pooled standard deviation of both groups
#' N is the number of experimental subjects of both groups (nE + nC)
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
                        n_experiments){
  e <- list()
  c <- list()

  for (i in 1:n_experiments) {
    e <- c(e, list(rnorm(sample_size, mean_e, sd_e)))
    c <- c(c, list(rnorm(sample_size, mean_c, sd_e)))
  }

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

parametric_rr_meta <- function(sample_size,
                               mean_e,
                               mean_c,
                               sd_e,
                               n_experiments){

  e <- list()
  c <- list()

  for (i in 1:n_experiments) {
    e <- c(e, list(rnorm(sample_size, mean_e, sd_e)))
    c <- c(c, list(rnorm(sample_size, mean_c, sd_e)))
  }

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
                      sm = "ROM",
                      fixed = TRUE)

  return(d$TE.fixed, d$lower.fixed, d$upper.fixed)

}

# implementacion propia en nuevo paquete , con interfaz parecida a la de META
non_parametric_rr_manual <- function(sample_size,
                                     mean_e,
                                     mean_c,
                                     n_experiments){

  rr <- mean_e / mean_c
  l_i <- log(rr)

  #v <- (sample_size * 2 / (sample_size ** 2) + log(rr**2) / sample_size * 4)

}

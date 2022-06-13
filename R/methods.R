# probar casos extremos para generar tests necesarios

#' Estimated overall treatment effect and
#' lower and upper confidence interval limits
#' using Hedges method (REFERENCE?)
#' add formula
#'
#' @param sample_size Number of observations
#' @param mean_e Estimated mean in experimental group
#' @param mean_c Estimated mean in control group
#' @param sd_e Standard deviation
#' @param num_experimentos
#'
#' @return effect size
#' @export
#'
#' @examples
hedges_meta <- function(sample_size,
                        mean_e,
                        mean_c,
                        sd_e,
                        num_experimentos){
  e <- list()
  c <- list()

  for (i in 1:num_experimentos) {
    e <- c(e, list(rnorm(sample_size, mean_e, sd_e)))
    c <- c(c, list(rnorm(sample_size, mean_c, sd_e)))
  }

  overall_sample_size <- rep(sample_size, num_experimentos)
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
                               num_experimentos){

  e <- list()
  c <- list()

  for (i in 1:num_experimentos) {
    e <- c(e, list(rnorm(sample_size, mean_e, sd_e)))
    c <- c(c, list(rnorm(sample_size, mean_c, sd_e)))
  }

  overall_sample_size <- rep(sample_size, num_experimentos)
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
                                     num_experimentos){

  rr <- mean_e / mean_c
  l_i <- log(rr)

  #v <- (sample_size * 2 / (sample_size ** 2) + log(rr**2) / sample_size * 4)

}

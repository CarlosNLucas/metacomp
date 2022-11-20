#' Calculation of accuracy, as in the empirical probability of the
#' meta-analysis method confidence interval (1 − α) containing the
#' population value δ
#'
#' @param results List of estimated effects bounds in format
#'  ( lower bound, upper bound )
#' @param population_effect Effect of the population
#'
#' @return Accuracy, in range 0-1
#' @export
#'
#' @examples
#' accuracy(list(c(1,3), c(-1, 4), c(1,4)), 3.2)
accuracy <- function(results, population_effect) {

  lower_bounds <- sapply(results, function(x) x[[1]])
  upper_bounds <- sapply(results, function(x) x[[2]])

  inside_interval <- length(which(population_effect >= lower_bounds &
                                  population_effect <= upper_bounds))

  inside_interval / length(results)
}


#' Calculation of empirical power, also known as acceptance rate,
#' for a list of estimated effects
#'
#'
#' @param results List of estimated effects bounds in format
#'  ( lower bound, upper bound )
#' @param population_effect Effect of the population
#'
#' @return Empirical power, in range 0-1
#' @export
#'
#' @examples
#' rejection_rate(list(c(1,3), c(-1, 4), c(1,4)), 3.2)
empirical_power <- function(results, population_effect) {
  hedges_bound <- 0
  lower_bounds <- sapply(results, function(x) x[[1]])

  1 - (length(which(lower_bounds < hedges_bound)) / length(results))
}


#' Calculation of rejection rate for a list of estimated effects
#'
#'
#' @param results List of estimated effects bounds in format
#'  ( lower bound, upper bound )
#' @param population_effect Effect of the population
#'
#' @return Rate of rejection, in range 0-1
#' @export
#'
#' @examples
#' rejection_rate(list(list(c(1,3), c(-1, 4), c(1,4)), 3.2)
rejection_rate <- function(results, population_effect) {
  hedges_bound <- 0
  lower_bounds <- sapply(results, function(x) x[[1]])

  1 - (length(which(lower_bounds > hedges_bound)) / length(results))
}

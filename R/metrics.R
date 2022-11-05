#' Calculation of accuracy, as in the empirical probability of the
#' meta-analysis method confidence interval (1 − α) containing the
#' population value δ
#'
#' @param results List of estimated effects in format
#'  ( effect, lower bound, upper bound )
#' @param d Effect of the population
#'
#' @return Accuracy, in range 0-1
#' @export
#'
#' @examples
#' accuracy(list(c(2,1,3), c(3,-1, 4), c(2, 1,4)), 3.2)
#' accuracy(estimated_effects)
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
#' @param results List of estimated effects in format
#'  ( effect, lower bound, upper bound )
#'
#' @return Empirical power, in range 0-1
#' @export
#'
#' @examples
#' rejection_rate(list(c(2,1,3), c(3,-1, 4), c(2, 1,4)))
#' rejection_rate(estimated_effects)
empirical_power <- function(results) {

  1-(length(which(results[[2]] < 0))/length(results))
}


#' Calculation of rejection rate for a list of estimated effects
#'
#'
#' @param results List of estimated effects in format
#'  ( effect, lower bound, upper bound )
#'
#' @return Rate of rejection, in range 0-1
#' @export
#'
#' @examples
#' rejection_rate(list(c(2,1,3), c(3,-1, 4), c(2, 1,4)))
#' rejection_rate(estimated_effects)
rejection_rate <- function(results) {

  1-(length(which(results[[2]] > 0))/length(results))
}

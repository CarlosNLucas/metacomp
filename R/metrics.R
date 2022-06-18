# TODO: docs
accuracy <- function(results, d) {

  inside_interval <- length(which(d >= results[[2]] &
                                    d <= results[[3]]))
  outside_interval <- length(results) - inside_interval

  list(inside_interval = inside_interval,
       outside_interval = outside_interval)
}


#' Calculation of empirical power, also known as acceptance rate,
#' for a list of estimated effects
#'
#'
#' @param results List of estimated effects in format
#'  [ effect, lower bound, upper bound ]
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
#'  [ effect, lower bound, upper bound ]
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

#' Title
#'
#' @return
#' @export
#'
#' @examples
pi_calculation_fun0 <- function() {
  x <- runif(get_num_veces())
  y <- runif(get_num_veces())
  ifelse(sqrt(x^2+y^2) <= 1, 1, 0)
}

#' Title
#'
#' @return
#' @export
#'
#' @examples
pi_calculation_fun1 <- function(results) {
  var_results <- var(results)
  4*sum(results[results == 1])/get_num_veces()
}

#' Title
#'
#' @return
#' @export
#'
#' @examples
pi_calculation_fun2 <- function(n) {
  1.96*sqrt(var(pi_calculation_fun0()))/sqrt(n)
}

#' Title
#'
#' @return
#' @export
#'
#' @examples
pi_calculation <- function() {
  set_num_veces(get_number_iterations(pi_calculation_fun2, 0.01))
  print(montecarlo(NULL, pi_calculation_fun0, pi_calculation_fun1))
}

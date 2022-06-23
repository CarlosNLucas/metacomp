
#' Plot a simple simulation with a single parameter
#'
#' @param matrizComb
#' @param method
#' @param metric
#' @param parameter
#'
#' @return a plot ?
#' @export
#'
#' @examples
plot <- function(matrizComb, method, metric, parameter, ...) {
  # TODO: check parameter is one inside matrizComb

  results <- montecarlo(matrizComb, method, metric)
  matrizComb$results <- unlist(results)

  ggplot2::ggplot(data=matrizComb, aes(x={{parameter}}, y=results, ...)) +
    geom_point() +
    geom_line()
}

plot_compare_methods <- function(matrizComb, results_1, results_2, parameter) {
  # ggplot(matrizComb, aes(x=n_experiments))+
  #   geom_line(aes(y=x, colour = "hedges")) +
  #   geom_line(aes(y=y, colour = "PRR"))
}


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
plot_sim <- function(matrizComb, method, metric, parameter, ...) {
  # TODO: check parameter is one inside matrizComb

  results <- montecarlo(matrizComb, method, metric)
  matrizComb$results <- unlist(results)

  ggplot2::ggplot(data=matrizComb, aes(x={{parameter}}, y=results, ...)) +
    geom_point() +
    geom_line()
}

plot <- function(matrizComb, results, parameter, ...){
  matrizComb$results <- unlist(results)

  ggplot2::ggplot(data=matrizComb, aes(x={{parameter}}, y=results, ...)) +
    geom_point() +
    geom_line()
}

plot_compare_methods <- function(matrizComb,
                                 method_1,
                                 method_2,
                                 metric,
                                 parameter,
                                 ...) {

  results_1 <- montecarlo(matrizComb, method_1, metric)
  results_2 <- montecarlo(matrizComb, method_2, metric)

  matrizComb$results_1 <- unlist(results_1)
  matrizComb$results_2 <- unlist(results_2)

  ggplot(matrizComb, aes(x={{parameter}}))+
    geom_line(aes(y=results_1, colour = "{{method_1}}", ...)) +
    geom_line(aes(y=results_2, colour = "{{method_2}}", ...))
}

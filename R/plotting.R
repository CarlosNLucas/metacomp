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

  ggplot2::ggplot(data=matrizComb, ggplot2::aes(x={{parameter}}, y=results, ...)) +
    ggplot2::geom_point() +
    ggplot2::geom_line()
}

#' Plot
#'
#' @param matrizComb
#' @param method
#' @param parameter
#'
#' @return
#' @export
#'
#' @examples
plot_compare_methods <- function(matrizComb,
                                 metric,
                                 parameter,
                                 ...) {

  require(ggplot2)

  metric_name  <- as.character(substitute(metric))
  metric_name <- paste(metric_name[2], "::", metric_name[3], sep="")

  parameter <- sym(parameter)
  metric_name <- sym(metric_name)

  ggplot(matrizComb, aes(x = !!parameter,
                         y = !!metric_name,
                         group = methods,
                         colour = methods)) +
  stat_summary(geom = "line",
               fun = mean)
}

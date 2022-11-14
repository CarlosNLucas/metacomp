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

  parameter <- sym(parameter)
  metric <- sym(metric)

  p <- ggplot(matrizComb, aes(x = !!parameter,
                         y = !!metric,
                         group = method,
                         colour = method)) +
  stat_summary(geom = "line",
               fun = mean)

  print(p)
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
plot_choose_method <- function(equivalences, ...) {

  require(ggplot2)
  require(scales)

col1name <- sym(names(equivalences)[1])
col2name <- sym(names(equivalences)[2])
p <- ggplot(equivalences, aes(x=!!col1name,
                          y=!!col2name)) +
  geom_point() +
  scale_y_continuous(breaks= pretty_breaks()) +
  geom_smooth(method = lm, aes(colour="regression line"))

print(p)
}

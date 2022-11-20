#' Plot a comparison of methods
#'
#' @param matrix_comb methods and comparison information
#' @param metric metric used in comparison to plot in vertical axis
#' @param parameter parameter of interest to plot in horizontal axis
#'
#' @return Graphical plot
#' @export
#'
plot_compare_methods <- function(matrix_comb,
                                 metric,
                                 parameter,
                                 ...) {
  require(ggplot2)

  parameter <- sym(parameter)
  metric <- sym(metric)

  p <- ggplot(matrix_comb, aes(
    x = !!parameter,
    y = !!metric,
    group = method,
    colour = method
  )) +
    stat_summary(
      geom = "line",
      fun = mean,
      size = 1
    ) +
    scale_y_continuous(
      expand = c(0, 0),
      limits = c(0, 1.05),
      breaks = scales::pretty_breaks(n = 11)
    )

  print(p)
}

#' Plot metric equivalences between two methods
#'
#' @param equivalences metric equivalences between two methods
#'
#' @return a plot
#' @export
plot_choose_method <- function(equivalences, ...) {
  require(ggplot2)
  require(scales)

  col1name <- sym(names(equivalences)[1])
  col2name <- sym(names(equivalences)[2])
  p <- ggplot(equivalences, aes(
    x = !!col1name,
    y = !!col2name
  )) +
    geom_point() +
    scale_y_continuous(breaks = pretty_breaks()) +
    geom_smooth(method = lm, aes(colour = "regression line"))

  print(p)
}

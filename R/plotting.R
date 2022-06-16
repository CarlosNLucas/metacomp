
plot <- function(matrizComb, results, parameter){
  #check parameter is one inside matrizComb
  matrizComb$x <- unlist(results)

  ggplot2::ggplot(data=matrizComb, aes(x={{parameter}}, y=x)) +
    geom_point() +
    geom_line()
}

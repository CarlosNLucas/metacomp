# comprobar si d poblacional esta dentro del intervalo
accuracy <- function(results, sample_size, d) {

  sd_d <- sqrt(
    ((sample_size + sample_size) / sample_size^2) +
      (d^2 / (2 * (sample_size + sample_size)))
  )

  #obtener intervalos de funciones del paquete
  inside_interval <- length(which(results >= (d - 1.96 * sd_d) &
                                    results <= (d + 1.96 * sd_d)))
  outside_interval <- length(results) - inside_interval

  list(inside_interval = inside_interval,
       outside_interval = outside_interval)
}


empirical_power <- function(results) {

  # sd <- sqrt(
  #   ((sample_size + sample_size) / sample_size^2) +
  #     (results^2 / (2 * (sample_size + sample_size)))
  # )

  #obtener intervalos de funciones del paquete
  #lower_bound <- results - 1.96 * sd

  list(1-(length(which(results[[2]] < 0))/length(results)))
}


rejection_rate <- function(results) {

  list(1-(length(which(results[[2]] > 0))/length(results)))
}

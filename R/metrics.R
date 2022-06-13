accuracy <- function(results, d) {

  inside_interval <- length(which(d >= results[[2]] &
                                    d <= results[[3]]))
  outside_interval <- length(results) - inside_interval

  list(inside_interval = inside_interval,
       outside_interval = outside_interval)
}


empirical_power <- function(results) {

  list(1-(length(which(results[[2]] < 0))/length(results)))
}


rejection_rate <- function(results) {

  list(1-(length(which(results[[2]] > 0))/length(results)))
}

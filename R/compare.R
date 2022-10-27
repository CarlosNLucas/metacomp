compare_methods <- function(methods, metric,
                            min_n_experiments, max_n_experiments,
                            min_sample_size, max_sample_size,
                            mean_e, mean_c, sd_e, sd_c,
                            overall = TRUE) {

  step_sample_size = round((max_sample_size - min_sample_size)/10)
  step_n_experiments = round((max_n_experiments - min_n_experiments)/10)
  parameter_matrix <-
    expand.grid(
      sample_size = seq(min_sample_size, max_sample_size, step_sample_size),
      mean_e = mean_e,
      sd_e = sd_e,
      mean_c = mean_c,
      sd_c = sd_c,
      n_experiments = seq(min_n_experiments, max_n_experiments, step_n_experiments),
      distribution = "normal",
      d = mean_e / mean_c
    )

  results <- list()
  #loop on methods
  for (i in 1:length(methods)) {
    results <- c(results, list(montecarlo(parameter_matrix, methods[[i]], metric)))
  }

  if(overall){
    scores <- list()
    #Get average from each result array
    for (i in 1:length(results)) {
      scores <- c(scores, mean(unlist(results[[i]])))
    }

    return(scores)
  }
  else{
    return(results)
  }
}


fun2 <- function(methods, metric,
                 n_variance, n_mean, n_unknown,
                 parameter_matrix){

}

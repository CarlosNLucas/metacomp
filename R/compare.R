#' Title
#'
#' @return
#' @export
#'
#' @examples
compare <- function(methods,
                    metric,
                    min_n_experiments, max_n_experiments,
                    min_sample_size, max_sample_size,
                    mean_e, mean_c, sd_e, sd_c,
                    distribution = "normal",
                    overall = TRUE) {

  parameter_matrix <-
    expand.grid(
      sample_size = seq(min_sample_size, max_sample_size),
      mean_e = mean_e,
      sd_e = sd_e,
      mean_c = mean_c,
      sd_c = sd_c,
      n_experiments = seq(min_n_experiments, max_n_experiments),
      distribution = distribution
    )

  results <- list()
  #loop on methods
  for (i in 1:length(methods)) {
    results <- c(results, list(montecarlo_meta(parameter_matrix, methods[[i]], metric)))
  }

  method_names <- as.character(substitute(methods))
  method_names <- method_names[2:length(method_names)]
  metric_name  <- as.character(substitute(metric))
  metric_name <- paste(metric_name[2], "::", metric_name[3], sep="")

  if(overall){
    scores <- c()
    #Get average from each result array
    for (i in 1:length(methods)) {
      scores <- c(scores, mean(unlist(results[[i]])))
    }
    return(cbind(method_names, scores))

    } else{

      parameter_matrix["methods"] <- NA
      parameter_matrix[[metric_name]] <- NA

      tmp_methods <- c()
      tmp_metric  <- c()

      for (i in 1:length(methods)) {
        tmp_metric <- c(tmp_metric, unlist(results[[i]]))
        tmp_methods <- c(tmp_methods, rep(method_names[i], length(unlist(results[[i]]))))
      }

      tmp_parameter_matrix <- parameter_matrix
      for (i in 1:(length(methods)-1)) {
        parameter_matrix <- rbind(parameter_matrix, tmp_parameter_matrix)
      }

      parameter_matrix[["methods"]] <- tmp_methods
      parameter_matrix[[metric_name]] <- tmp_metric
      return(parameter_matrix)
  }
}


fun2 <- function(methods, metric,
                 n_variance, n_mean, n_unknown,
                 parameter_matrix){

}

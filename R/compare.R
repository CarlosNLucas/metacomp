#'  Compare a list of methods for a metric given a group of population values
#'
#' @param methods list of methods
#' @param metric metric to evaluate
#' @param min_n_experiments minimum number of experiments
#' @param max_n_experiments maximum number of experiments
#' @param min_sample_size minimum number of sample size
#' @param max_sample_size maximum number of sample size
#' @param mean_e mean in experimental group
#' @param mean_c mean in control group
#' @param sd_e Standard deviation in experimental group
#' @param sd_c dard deviation in control group
#' @param distribution Distibution model
#' @param overall Overall effect
#'
#' @return results and parameters matrix
#' @export
#'
compare <- function(methods,
                    metric,
                    min_n_experiments,
                    max_n_experiments,
                    min_sample_size,
                    max_sample_size,
                    mean_e,
                    mean_c,
                    sd_e,
                    sd_c,
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
      distribution = distribution,
      sample_size_e = 0,
      sample_size_c = 0,
      log_transformed = TRUE,
      alpha_level = 0.05
    )

  # Propagate values
  for (i in seq_len(nrow(parameter_matrix))) {
    parameter_matrix[i, ]$sample_size_e <- parameter_matrix[i, ]$sample_size
    parameter_matrix[i, ]$sample_size_c <- parameter_matrix[i, ]$sample_size
  }

  results <- list()
  #loop on methods
  for (i in seq_along(methods)) {
    results <- c(results, list(montecarlo_meta(parameter_matrix, methods[[i]], metric)))
  }

  if (overall) {
    scores <- c()
    #Get average from each result array
    for (i in seq_along(methods)) {
      scores <- c(scores, mean(unlist(results[[i]])))
    }
    return(cbind(methods, scores))

    } else {

      parameter_matrix["method"] <- NA
      parameter_matrix[[metric]] <- NA

      tmp_methods <- c()
      tmp_metric  <- c()

      for (i in seq_along(methods)) {
        tmp_metric <- c(tmp_metric, unlist(results[[i]]))
        tmp_methods <- c(tmp_methods, rep(methods[i], length(unlist(results[[i]]))))
      }

      tmp_parameter_matrix <- parameter_matrix
      for (i in 1:(length(methods) - 1)) {
        parameter_matrix <- rbind(parameter_matrix, tmp_parameter_matrix)
      }

      parameter_matrix[["method"]] <- tmp_methods
      parameter_matrix[[metric]] <- tmp_metric
      return(parameter_matrix)
  }
}


fun2 <- function(methods, metric,
                 n_variance, n_mean, n_unknown,
                 parameter_matrix) {

}


#' Compare two methods and find points of equivalence
#'
#' @param methods list of methods
#' @param metric metric to evaluate
#' @param min_n_experiments minimum number of experiments
#' @param max_n_experiments maximum number of experiments
#' @param min_sample_size minimum number of sample size
#' @param max_sample_size maximum number of sample size
#' @param mean_e mean in experimental group
#' @param mean_c mean in control group
#' @param sd_e Standard deviation in experimental group
#' @param sd_c dard deviation in control group
#' @param distribution Distibution model
#' @param parameter_of_interest parameter of interest
#'
#' @return points of equivalence
#' @export
decide <- function(methods,
                   metric,
                   min_n_experiments,
                   max_n_experiments,
                   min_sample_size,
                   max_sample_size,
                   mean_e,
                   mean_c,
                   sd_e,
                   sd_c,
                   distribution = "normal",
                   parameter_of_interest = "") {

  if (parameter_of_interest == "")
    stop("The parameter for decision making has to be explicitly indicated")

  comparison_results <- metacomp::compare(methods,
                                metric,
                                min_n_experiments, max_n_experiments,
                                min_sample_size, max_sample_size,
                                mean_e, mean_c, sd_e, sd_c,
                                distribution = "normal",
                                overall = FALSE)

  if (!(parameter_of_interest %in% names(comparison_results)))
    stop("The specified parameter does not appear in the results")

  tmp_results <- comparison_results[c("method", parameter_of_interest)]
  tmp_results <- tmp_results[!duplicated(tmp_results),]
  tmp_mean <- c()
  for(i in 1:nrow(tmp_results)) {
    tmp_mean <- c(tmp_mean, mean(comparison_results[
                                                      comparison_results$method == tmp_results[i,]$method &
                                                      comparison_results[[parameter_of_interest]] == tmp_results[i,][[parameter_of_interest]],
                                                   ][[metric]]
                                )
                 )
  }

  results <- tmp_results
  results[[metric]] <- tmp_mean

  what_compare <- results[results$method == methods[2],]
  compare_to <- results[results$method == methods[1],]

  values <- c()
  equivalences <- data.frame(A= numeric(0), B= numeric(0))
  col1name <- paste(methods[1], parameter_of_interest, sep="_")
  col2name <- paste(methods[2], parameter_of_interest, sep="_")
  names(equivalences) <- c(col1name, col2name)

  for (i in 1:nrow(what_compare)) {
    value <- compare_to[which.min(abs(what_compare[i,][[metric]] - compare_to[[metric]])),][[parameter_of_interest]]
    equivalences[nrow(equivalences)+1, ] <- c(value, what_compare[i,][[parameter_of_interest]])
  }

  equivalences

}

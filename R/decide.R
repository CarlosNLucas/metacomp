#' Title
#'
#' @return
#' @export
#'
#' @examples
decide <- function(methods,
                   metric,
                   min_n_experiments, max_n_experiments,
                   min_sample_size, max_sample_size,
                   mean_e, mean_c, sd_e, sd_c,
                   distribution = "normal",
                   parameter_of_interest = "") {

  if(parameter_of_interest == "")
    error("The paraneter for decision making has to be explicitly indicated")

  comparison_results <- metacomp::compare(methods,
                                metric,
                                min_n_experiments, max_n_experiments,
                                min_sample_size, max_sample_size,
                                mean_e, mean_c, sd_e, sd_c,
                                distribution = "normal",
                                overall = FALSE)

  if(!(parameter_of_interest %in% names(comparison_results)))
    error("The specified paraneter does not appear in the results")

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

  for(i in 1:nrow(what_compare)) {
    value <- compare_to[which.min(abs(what_compare[i,][[metric]] - compare_to[[metric]])),][[parameter_of_interest]]
    equivalences[nrow(equivalences)+1, ] <- c(value, what_compare[i,][[parameter_of_interest]])
  }

  equivalences

}

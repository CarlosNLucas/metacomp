varEnv <- new.env()
varEnv$NUM_VECES <- 50

set_num_veces <- function(val) {
  if (class(val) != "numeric") {
    stop("Value must be a number")
  }

  val <- abs(as.integer(val))

  varEnv$NUM_VECES <- val
}

get_num_veces <- function(){
  #get(NUM_VECES, envir = varEnv)
  varEnv$NUM_VECES
}

#' Title
#'
#' @param matrizComb
#' @param fun0
#' @param fun1
#'
#' @return
#' @export
#'
#' @examples
montecarlo <- function(matrizComb, fun0, fun1, store=FALSE) {
  # extract function parameters
  params_fun0 <- formals(fun0)
  params_fun1 <- as.list(formals(fun1))

  # function output
  output <- list()

  # we run the function for each combination of the input matrix
  for (i in 1:nrow(matrizComb)) {
    # store the results of the simulations. Each row of the input matrix
    # will be processed NUM_VECES
    #results <- rep(NA, NUM_VECES)
    results <- list()

    for (j in 1:varEnv$NUM_VECES) {
      # names extract the param names, making unnecessary to know them in the package
      for (param in names(params_fun0)) {
        # TODO: comprobar si estan los parametros adecuados
        params_fun0[param] <- matrizComb[i, ][param]
      }

      # run fun0 using passing its parameters as a list
      results <- c(results, list(do.call(fun0, params_fun0)))
    }

    # we do the same with fun1, but the results vector is internal, so we
    # cannot search the input matrix for it
    for (param in names(params_fun1)) {
      if(param != "results")
        params_fun1[param] <- matrizComb[i, ][param]
    }

    # same as before, but we add the results vector
    params_fun1$results <- results
    #print(typeof(params_fun1))
    output[[i]] <- do.call(fun1, params_fun1)
  }

  if (store) {
    #TODO: escribir en batch cada X calculos de la matriz?
    #      csv, RDA, ...?
    matrizComb$results <- unlist(output)
    write.csv(matrizComb, "experiment_data.csv", row.names = TRUE)
  }

  output

}

# TODO: adaptar a uso montecarlo(matriz, metodo = "hedges", "accuracy")
# nivel adicional de asbtraccion
# x <- montecarlo(matrizComb, parametric_rr_meta, empirical_power)

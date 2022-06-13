#esto tiene que ir en la inicializacion de la libreria. Tenemos que
# crear una funcion que actualice el valor.
NUM_VECES <- 10


montecarlo <- function(matrizComb, fun0, fun1) {
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

    for (j in 1:NUM_VECES) {
      # names extract the param names, making unnecessary to know them in the package
      for (param in names(params_fun0)) {
        #comprobar si estan los parametros adecuados
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

    #print(results)
    # same as before, but we add the results vector
    params_fun1$results <- results
    #print(typeof(params_fun1))
    output[[i]] <- do.call(fun1, params_fun1)
  }

  output

}

# adaptar a uso montecarlo(matriz, metodo = "hedges", "accuracy")
# nivel adicional de asbtraccion
# x <- montecarlo(matrizComb, parametric_rr_meta, empirical_power)

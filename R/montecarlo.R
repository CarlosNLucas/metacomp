varEnv <- new.env()
varEnv$NUM_VECES <- 50

#' Title
#'
#' @return
#' @export
#'
#' @examples
set_num_veces <- function(val) {
  if (class(val) != "numeric" && class(val) != "integer") {
    stop("Value must be a number")
  }

  val <- abs(as.integer(val))

  varEnv$NUM_VECES <- val
}

#' Title
#'
#' @return
#' @export
#'
#' @examples
get_num_veces <- function(){
  #get(NUM_VECES, envir = varEnv)
  varEnv$NUM_VECES
}

#' Title
#'
#' @return
#' @export
#'
#' @examples
get_number_iterations <- function(fun2, required_error, n_increment = 10, n_limit=+Inf) {
  error <- +Inf
  n <- 0
  while(error > required_error && n < n_limit) {
    n <- n + n_increment
    set_num_veces(n)
    error <- fun2(n)
  }
  n
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
  if(class(fun0) != 'function'){
    error("Method must be function")
  }
  if(class(fun1) != 'function'){
    error("Metric must be function")
  }


  # extract function parameters
  params_fun0 <- as.list(formals(fun0))
  params_fun1 <- as.list(formals(fun1))

  # function output
  output <- list()

  # we run the function for each combination of the input matrix
  # if the input matrix is null, we run the loop just once
  if(is.null(nrow(matrizComb)))
    times <- 1
  else
    times <- nrow(matrizComb)

  for (i in 1:times) {
    # store the results of the simulations. Each row of the input matrix
    # will be processed NUM_VECES
    #results <- rep(NA, NUM_VECES)
    results <- list()

    # names extract the param names, making unnecessary to know them in the package
    for (param in names(params_fun0)) {
      # TODO: comprobar si estan los parametros adecuados
      params_fun0[param] <- matrizComb[i, ][param]
    }

    # run fun0 using passing its parameters as a list
    results <- c(results, list(do.call(fun0, params_fun0)))

    # we do the same with fun1, but the results vector is internal, so we
    # cannot search the input matrix for it
    for (param in names(params_fun1)) {
      if(param != "results")
        params_fun1[param] <- matrizComb[i, ][param]
    }

    # same as before, but we add the results vector
    params_fun1$results <- unlist(results)
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

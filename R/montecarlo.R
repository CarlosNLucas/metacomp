# Create environment for variables
var_env <- new.env()
# Variable n_iterations set initally to 10
var_env$n_iterations <- 10

#' Set the number of iterations the algorithm runs
#'
#' @param val Number of iterations
#'
#' @export
#'
#' @examples set_n_iterations(200)
set_n_iterations <- function(val) {
  if (class(val) != "numeric" && class(val) != "integer") {
    stop("Value must be a number")
  }
  if (val < 2) {
    stop("Value must be 2 or higher")
  }
  val <- as.integer(val)

  var_env$n_iterations <- val
}

#' Get the number of iterations the algorithm runs
#'
#' @return number of iterations
#' @export
#'
#' @examples get_n_iterations()
get_n_iterations <- function() {
  var_env$n_iterations
}


get_number_iterations <- function(fun2, required_error, n_increment = 10, n_limit = +Inf) {
  error <- +Inf
  n <- 0
  while (error > required_error && n < n_limit) {
    n <- n + n_increment
    set_n_iterations(n)
    error <- fun2(n)
  }
  n
}

#' Perform Monte Carlo simulations. See examples
#'
#' @param matrix_comb Population parameters
#' @param random_generation_function random generation function
#' @param metric_calculation_function metric calculation function
#'
#' @return Results of Monte Carlo simulations
#' @export
#'
#' @examples
#' # Example 1: Calculation of the value of pi
#'
#' # Random process underlying the calculation of pi
#' # using the area method
#' # see, e.g., https://blogs.sas.com/content/iml/2016/03/14/monte-carlo-estimates-of-pi.html
#'
#' fun0 <- function() {
#'   x <- runif(metacomp::get_n_iterations())
#'   y <- runif(metacomp::get_n_iterations())
#'   ifelse(sqrt(x^2 + y^2) <= 1, 1, 0)
#' }
#'
#' # Post-processing, in this case, the calculation of pi
#' # The results list is provided by the metacomp::montecarlo function
#' # results contains the output of fun0()
#'
#' fun1 <- function(results) {
#'   4 * sum(results[results == 1]) / metacomp::get_n_iterations()
#' }
#'
#' # We set the number of samples to 1000
#' metacomp::set_n_iterations(1000)
#'
#' # and make the calculation
#' metacomp::montecarlo(NULL, fun0, fun1)
#'
#' # Please notice that metacomp::montecarlo() return a list of lists
#' # Each list element is the outcome of a different simulation
#' # This can be handy in some cases, e.g., see below.
#'
#'
#' # Example 2: Calculation of a definite integral
#'
#'
#' # Random process underlying the calculation of an integral
#' # see, e.g., https://blogs.sas.com/content/iml/2021/03/31/estimate-integral-monte-carlo.html
#'
#' fun0 <- function(a, b, n) {
#'   # This time we don't use the metacomp::get_n_iterations()
#'   runif(n, a, b)
#' }
#'
#' # Post-processing, where the area is calculated
#'
#' fun1 <- function(a, b, results) {
#'   # Take care of the order of the parameters because f receives 2
#'   (b - a) * mean(as.numeric(lapply(results, f, a = 4)))
#' }
#'
#' # The function to integrate should be defined somewhere
#'
#' f <- function(x, a) {
#'   x^(a - 1) * exp(-x)
#' }
#'
#' # We can pass additional parameters to metacomp::montecarlo() using a matrix
#'
#' m <- expand.grid(
#'   a = 1,
#'   b = 3.5,
#'   n = c(1000, 10000, 100000)
#' )
#'
#'
#' # We make the calculation. Notice that the integral is calculated
#' # several times. Each row of m corresponds with a different calculation
#' metacomp::montecarlo(m, fun0, fun1)
#'
montecarlo <- function(matrix_comb,
                       random_generation_function,
                       metric_calculation_function) {
  fun0 <- get(random_generation_function)
  fun1 <- get(metric_calculation_function)

  if (class(fun0) != "function") {
    stop("Method must be function")
  }
  if (class(fun1) != "function") {
    stop("Metric must be function")
  }

  # extract function parameters
  params_fun0 <- as.list(formals(fun0))
  params_fun1 <- as.list(formals(fun1))

  # function output
  output <- list()

  # we run the function for each combination of the input matrix
  # if the input matrix is null, we run the loop just once
  if (is.null(nrow(matrix_comb))) {
    times <- 1
  } else {
    times <- nrow(matrix_comb)
  }

  for (i in 1:times) {
    # store the results of the simulations. Each row of the input matrix
    # will be processed n_iterations
    results <- list()

    # names extract the param names, making unnecessary to know them in the package
    for (param in names(params_fun0)) {
      params_fun0[param] <- matrix_comb[i, ][param]
    }

    # run fun0 using passing its parameters as a list
    results <- c(results, list(do.call(fun0, params_fun0)))

    # we do the same with fun1, but the results vector is internal, so we
    # cannot search the input matrix for it
    for (param in names(params_fun1)) {
      if (param != "results" && param != "effect_size") {
        params_fun1[param] <- matrix_comb[i, ][param]
      }
    }

    # same as before, but we add the results vector
    params_fun1$results <- results
    params_fun1$effect_size <- effect_size
    output[[i]] <- do.call(fun1, params_fun1)
  }

  output
}



#' Perform Monte Carlo simulations. See examples
#'
#' @param matrix_comb Population parameters
#' @param random_generation_function random generation function
#' @param metric_calculation_function metric calculation function
#'
#' @return Results of Monte Carlo simulations
#' @export
#'
#' @examples
#'
#' # Example 1: Calculation of the value of pi
#'
#' # Random process underlying the calculation of pi
#' # using the area method
#' # see, e.g., https://blogs.sas.com/content/iml/2016/03/14/monte-carlo-estimates-of-pi.html
#'
#' fun0 <- function() {
#'   x <- runif(metacomp::get_n_iterations())
#'   y <- runif(metacomp::get_n_iterations())
#'   ifelse(sqrt(x^2 + y^2) <= 1, 1, 0)
#' }
#'
#' # Post-processing, in this case, the calculation of pi
#' # The results list is provided by the metacomp::montecarlo function
#' # results contains the output of fun0()
#'
#' fun1 <- function(results) {
#'   4 * sum(results[results == 1]) / metacomp::get_n_iterations()
#' }
#'
#' # We set the number of samples to 1000
#' metacomp::set_n_iterations(1000)
#'
#' # and make the calculation
#' metacomp::montecarlo(NULL, fun0, fun1)
#'
#' # Please notice that metacomp::montecarlo() return a list of lists
#' # Each list element is the outcome of a different simulation
#' # This can be handy in some cases, e.g., see below.
#'
#'
#' # Example 2: Calculation of a definite integral
#'
#'
#' # Random process underlying the calculation of an integral
#' # see, e.g., https://blogs.sas.com/content/iml/2021/03/31/estimate-integral-monte-carlo.html
#'
#' fun0 <- function(a, b, n) {
#'   # This time we don't use the metacomp::get_n_iterations()
#'   runif(n, a, b)
#' }
#'
#' # Post-processing, where the area is calculated
#'
#' fun1 <- function(a, b, results) {
#'   # Take care of the order of the parameters because f receives 2
#'   (b - a) * mean(as.numeric(lapply(results, f, a = 4)))
#' }
#'
#' # The function to integrate should be defined somewhere
#'
#' f <- function(x, a) {
#'   x^(a - 1) * exp(-x)
#' }
#'
#' # We can pass additional parameters to metacomp::montecarlo() using a matrix
#'
#' m <- expand.grid(
#'   a = 1,
#'   b = 3.5,
#'   n = c(1000, 10000, 100000)
#' )
#'
#'
#' # We make the calculation. Notice that the integral is calculated
#' # several times. Each row of m corresponds with a different calculation
#' metacomp::montecarlo(m, fun0, fun1)
montecarlo_meta <- function(matrix_comb,
                            random_generation_function,
                            metric_calculation_function) {
  fun0 <- get(random_generation_function)
  fun1 <- get(metric_calculation_function)

  if (class(fun0) != "function") {
    stop("Method must be function")
  }
  if (class(fun1) != "function") {
    stop("Metric must be function")
  }

  # extract function parameters
  params_fun0 <- as.list(formals(fun0))
  params_fun1 <- as.list(formals(fun1))

  # function output
  output <- list()

  # we run the function for each combination of the input matrix
  # if the input matrix is null, we run the loop just once
  if (is.null(nrow(matrix_comb))) {
    times <- 1
  } else {
    times <- nrow(matrix_comb)
  }

  for (i in 1:times) {
    # names extract the param names, making unnecessary to know them in the package
    for (param in names(params_fun0)) {
      if (param != "return") {
        params_fun0[param] <- matrix_comb[i, ][param]
      }
    }

    results <- list()

    # run fun0 using passing its parameters as a list
    params_fun0$return <- "population effect"
    population_effect <- do.call(fun0, params_fun0)

    params_fun0$return <- "limits"
    for (j in 1:get_n_iterations()) {
      results[[j]] <- do.call(fun0, params_fun0)
    }

    # we do the same with fun1, but the results vector is internal, so we
    # cannot search the input matrix for it
    for (param in names(params_fun1)) {
      if (param != "results" && param != "population_effect") {
        params_fun1[param] <- matrix_comb[i, ][param]
      }
    }

    # same as before, but we add the results vector
    params_fun1[["results"]] <- results
    params_fun1[["population_effect"]] <- population_effect
    output[[i]] <- do.call(fun1, params_fun1)
  }
  output
}

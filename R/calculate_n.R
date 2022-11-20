

#' Get ideal number of iterations for a Monte Carlo simulation
#'
#' @param required_error Required error
#' @param fun0 Generation function
#' @param fun2 Escalating function
#' @param tolerance tolerance for the error
#' @param confidence_interval confidence interval for the error
#' @param initial_n optional initial value for n
#'
#' @return Number of iterations
#' @export
#'
#' @examples
#'
#' fun0 <- function() {
#'  x <- runif(metacomp::get_n_iterations())
#'  y <- runif(metacomp::get_n_iterations())
#'  ifelse(sqrt(x^2 + y^2) <= 1, 1, 0)
#' }
#'
#' fun1 <- function(results) {
#'   4 * sum(results[results == 1]) / metacomp::get_n_iterations()
#' }
#'
#' fun2 <- function(results) {
#'   16 * var(results)
#' }
#'
#' x <- get_n_for_prespecified_error(0.01, fun0, fun2)
#'
get_n_for_prespecified_error <- function(required_error,
                                         fun0,
                                         fun2,
                                         tolerance = 0.1,
                                         confidence_interval = .95,
                                         initial_n = 10000) {
    # The user can specify the number of interactions
    # from which to start. They may have an idea given
    # the error margins they want to achieve
    if (initial_n != round(initial_n)) {
        warning("initial_n is not integer. We have rounded it")
        initial_n <- round(initial_n)
    }

    if (initial_n <= 0) {
        stop("initial_n cannot be smaller than or equal to zero")
    } else {
        set_n_iterations(initial_n)
    }

    # The difference between the required and the actual error
    # is the stop criterion. actual_error < required_error
    # can lead to too many iterations
    #
    # The tolerance specifies the relative difference between
    # actual_error and required_error to stop the search
    margin <- required_error * tolerance
    print(margin)

    # A simple binary search
    # We use the get_n_iterations() and set_n_iterations() functions
    # to leave the packlage in the correct state

    previous_n <- 0
    repeat{
        actual_error <- get_error(get_variance(NULL, fun0, fun2), confidence_interval)

        print(abs(actual_error - required_error))

        if (abs(actual_error - required_error) <= margin) {
            break
        }

        if (actual_error > required_error) {
            new_n <- get_n_iterations() * 2
        } else {
            new_n <- (get_n_iterations() + previous_n) %/% 2
        }

        print(new_n)
        previous_n <- get_n_iterations()
        set_n_iterations(new_n)
    }

    get_n_iterations()
}

get_variance <- function(parameters,
                         fun0,
                         fun2) {
    # Parameters should be a matrix or null.
    # Otherwise, we raise an error
    # By the way: a matrix with no rows is null
    type <- class(parameters)
    if (type[1] == "NULL" || type[1] == "matrix") {
        # if parameters contains more than one row, we raise
        # a warning but continue
        if (type[1] == "matrix" && nrow(parameters) > 1) {
            warning("<parameters> contains several rows. Only the first one will be used")
        }
    } else {
        stop("<parameters> should be a matrix or null")
    }

    # Obtain the function arguments
    # The loop assigns parameters to argumentd, making
    # unnecessary to know the arguments in the package
    params_fun0 <- as.list(formals(fun0))
    params_fun2 <- as.list(formals(fun2))
    for (param in names(params_fun0)) {
        tryCatch(
            params_fun0[param] <- parameters[1, ][param],
            # Let's provide significant information instead of an
            # abstract message
            stop("<fun0> has arguments that do not appear in the parameter list")
        )
    }

    # run fun0 using passing its parameters as a list
    results <- do.call(fun0, params_fun0)

    # we do the same with fun2, but the results variable
    # is internal, so we cannot search the parameters for it
    for (param in names(params_fun2)) {
        if (param != "results") {
            tryCatch(
                params_fun2[param] <- parameters[1, ][param],
                # Let's provide significant information instead of an
                # abstract message
                stop("<fun2> has arguments that do not appear in the parameter list")
            )
        }
    }

    # same as before, but we add the results vector
    params_fun2$results <- results
    do.call(fun2, params_fun2)
}

get_error <- function(variance, confidence_interval = .95) {
    # The confidence interval is 2-tailed
    sqrt(variance) * qnorm((1 - confidence_interval) / 2 + confidence_interval) / sqrt(get_n_iterations())
}

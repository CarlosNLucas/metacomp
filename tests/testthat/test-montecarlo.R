matrix_test <-
  expand.grid(
    sample_size = 5,
    mean_e = 10,
    sd_e = 3,
    mean_c = 8,
    sd_c = 3,
    n_experiments = seq(1, 21, 2),
    distribution = "lognormal",
    d = 1/3)

matrix_incomplete <-
  expand.grid(
    mean_e = 10,
    sd_e = 3,
    mean_c = 8,
    n_experiments = seq(1, 21, 2),
    distribution = "lognormal",
    d = 1/3)

not_a_matrix <- "foul"

#Sanity check
test_that("get_n_iterations works", {
  expect_equal(var_env$n_iterations, get_n_iterations())
})

#MONT-2
test_that("set_n_iterations works", {
  num <- 21
  set_n_iterations(num)
  expect_equal(var_env$n_iterations, num)
})

#MONT-7
test_that("n_iterations is not Integer or Numerical", {
  not_num <- "21"
  expect_error(set_n_iterations(num))
})

# Set variable to low number for short loop execution
prev_val <- get_n_iterations()
set_n_iterations(10)

# MONT-1
test_that("montecarlo works", {
  expect_success(expect_type(
                    montecarlo(matrix_test,
                                parametric_rr_meta,
                                empirical_power),
                    'list'))
})

#MONT-3
test_that("matrix is not dataframe", {
  expect_error(montecarlo(not_a_matrix,
                          parametric_rr_meta,
                          empirical_power))
})


#MONT-4
test_that("matrix is missing values", {
  expect_error(montecarlo(matrix_incomplete,
                          parametric_rr_meta,
                          empirical_power))
})

#MONT-5
test_that("method is not a function", {
  expect_error(montecarlo(matrix_test,
                          23,
                          empirical_power))
})

#MONT-6
test_that("metric is not a function", {
  expect_error(montecarlo(matrix_test,
                          hedges_meta,
                          123))
})

#MONT-?
test_that("method/metric parameters
          dont match matrix values", {
  expect_error(montecarlo(matrix_test,
                            function(these, params){},
                            function(not, in_matrix){}))
})

# -- Montecarlo Pi convergence test --

set_n_iterations(50)

pi_matrix <- expand.grid(n = c(100,1000),
                         test = "test") #  ??? Fix

pi_generation <- function(n){
  x <- runif(n)
  y <- runif(n)
  random_variable <- ifelse(sqrt(x^2+y^2) <= 1, 1, 0)
  var_random_variable <- var(random_variable)
  #print(var_random_variable)
  pi <- 4*sum(random_variable[random_variable == 1])/n
  result <- c(pi, 16*var_random_variable)
}

pi_error <- function(results, n){
  results_unl <- unlist(results)
  # Pi error 95%
  error <- 1.96*sqrt(16*(pi/4)*(1-pi/4))/sqrt(n)
  # Sample error 95%
  s_error <- lapply(results, function(x) 1.96*sqrt(x)/sqrt(n))

  # Using theoretical error for now
  lapply(results, function(x) x[1] < pi + error && x[1] > pi - error)
}

test_that("Montecarlo internally works", {
  #in loop <- really??
  output <- montecarlo(pi_matrix, pi_generation, pi_error)
  ratios <- c()

  #calculate ratio TRUE:total
  for(i in 1:length(pi_matrix$n)){
    values <- unlist(output[i])
    ratio <- sum(values==TRUE)/length(values)
    ratios <- c(ratios, ratio)
  }

  print(ratios)
  #check if ratio >= 95%
  test <- lapply(ratios, function(x) x >= 0.95)

  for(i in 1:length(test)){
    expect_true(unlist(test[i]))
  }
})

set_n_iterations(prev_val)

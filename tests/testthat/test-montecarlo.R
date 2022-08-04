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

test_that("get_num_veces works", {
  expect_equal(varEnv$NUM_VECES, get_num_veces())
})

test_that("set_num_veces works", {
  num <- 21
  set_num_veces(num)
  expect_equal(varEnv$NUM_VECES, num)
})

# Set variable to low number for short loop execution
prev_val <- get_num_veces()
set_num_veces(10)

test_that("montecarlo works", {
  expect_success(expect_type(
                    montecarlo(matrix_test,
                                parametric_rr_meta,
                                empirical_power),
                    'list'))
})

test_that("error when matrix is not dataframe", {
  expect_error(montecarlo(not_a_matrix,
                          parametric_rr_meta,
                          empirical_power))
})

test_that("error when matrix is missing values", {
  expect_error(montecarlo(matrix_incomplete,
                          parametric_rr_meta,
                          empirical_power))
})

test_that("error when method is not a function", {
  expect_error(montecarlo(matrix_test,
                          23,
                          empirical_power))
})

test_that("error when metric is not a function", {
  expect_error(montecarlo(matrix_test,
                          hedges_meta,
                          123))
})

test_that("error when method/metric parameters
          dont match matrix values", {
  expect_error(montecarlo(matrix_test,
                            function(these, params){},
                            function(not, in_matrix){}))
})

set_num_veces(prev_val)

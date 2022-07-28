l <- list(c(2,1,4), c(3,-1, 4), c(2, 1,4), c(3,-1, 2))

test_that("accuracy works", {
  expect_equal(accuracy(l, 2), 1)
})

test_that("empirical power works", {
  expect_equal(empirical_power(l), 0.75)
})

test_that("rejection rate works", {
  expect_equal(rejection_rate(l), 0.5)
})

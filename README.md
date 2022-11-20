<!-- toc -->

November 20, 2022

Carlos Nuñez - carlos.nunnezlucas@estudiante.uam.es

License: MIT + file LICENSE

# Compare Methods Of Meta-analysis
## Package: metacomp



```
Title: Compare Methods Of Meta-analysis
Version: 1.0
License: MIT + file LICENSE
Encoding: UTF-8
Roxygen: list(markdown = TRUE)
RoxygenNote: 7.2.1
Imports: 
    ggplot2,
    meta
Suggests: 
    testthat (>= 3.0.0)
Config/testthat/edition: 3
```


# `accuracy`

Calculation of accuracy, as in the empirical probability of the
 meta-analysis method confidence interval (1 − α) containing the
 population value δ


## Description

Calculation of accuracy, as in the empirical probability of the
 meta-analysis method confidence interval (1 − α) containing the
 population value δ


## Usage

```r
accuracy(results, population_effect)
```


## Arguments

Argument      |Description
------------- |----------------
`results`     |     List of estimated effects bounds in format ( lower bound, upper bound )
`population_effect`     |     Effect of the population


## Value

Accuracy, in range 0-1


## Examples

```r
accuracy(list(c(1,3), c(-1, 4), c(1,4)), 3.2)
```


# `compare`

Compare a list of methods for a metric given a group of population values


## Description

Compare a list of methods for a metric given a group of population values


## Usage

```r
compare(
  methods,
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
  overall = TRUE
)
```


## Arguments

Argument      |Description
------------- |----------------
`methods`     |     list of methods
`metric`     |     metric to evaluate
`min_n_experiments`     |     minimum number of experiments
`max_n_experiments`     |     maximum number of experiments
`min_sample_size`     |     minimum number of sample size
`max_sample_size`     |     maximum number of sample size
`mean_e`     |     mean in experimental group
`mean_c`     |     mean in control group
`sd_e`     |     Standard deviation in experimental group
`sd_c`     |     dard deviation in control group
`distribution`     |     Distibution model
`overall`     |     Overall effect


## Value

results and parameters matrix


# `decide`

Compare two methods and find points of equivalence


## Description

Compare two methods and find points of equivalence


## Usage

```r
decide(
  methods,
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
  parameter_of_interest = ""
)
```


## Arguments

Argument      |Description
------------- |----------------
`methods`     |     list of methods
`metric`     |     metric to evaluate
`min_n_experiments`     |     minimum number of experiments
`max_n_experiments`     |     maximum number of experiments
`min_sample_size`     |     minimum number of sample size
`max_sample_size`     |     maximum number of sample size
`mean_e`     |     mean in experimental group
`mean_c`     |     mean in control group
`sd_e`     |     Standard deviation in experimental group
`sd_c`     |     dard deviation in control group
`distribution`     |     Distibution model
`parameter_of_interest`     |     parameter of interest


## Value

points of equivalence


# `empirical_power`

Calculation of empirical power, also known as acceptance rate,
 for a list of estimated effects


## Description

Calculation of empirical power, also known as acceptance rate,
 for a list of estimated effects


## Usage

```r
empirical_power(results, population_effect)
```


## Arguments

Argument      |Description
------------- |----------------
`results`     |     List of estimated effects bounds in format ( lower bound, upper bound )
`population_effect`     |     Effect of the population


## Value

Empirical power, in range 0-1


## Examples

```r
rejection_rate(list(c(1,3), c(-1, 4), c(1,4)), 3.2)
```


# `get_n_for_prespecified_error`

Get ideal number of iterations for a Monte Carlo simulation


## Description

Get ideal number of iterations for a Monte Carlo simulation


## Usage

```r
get_n_for_prespecified_error(
  required_error,
  fun0,
  fun2,
  tolerance = 0.1,
  confidence_interval = 0.95,
  initial_n = 10000
)
```


## Arguments

Argument      |Description
------------- |----------------
`required_error`     |     Required error
`fun0`     |     Generation function
`fun2`     |     Escalating function
`tolerance`     |     tolerance for the error
`confidence_interval`     |     confidence interval for the error
`initial_n`     |     optional initial value for n


## Value

Number of iterations


## Examples

```r
fun0 <- function() {
x <- runif(metacomp::get_n_iterations())
y <- runif(metacomp::get_n_iterations())
ifelse(sqrt(x^2 + y^2) <= 1, 1, 0)
}

fun1 <- function(results) {
4 * sum(results[results == 1]) / metacomp::get_n_iterations()
}

fun2 <- function(results) {
16 * var(results)
}

x <- get_n_for_prespecified_error(0.01, fun0, fun2)
```


# `get_n_iterations`

Get the number of iterations the algorithm runs


## Description

Get the number of iterations the algorithm runs


## Usage

```r
get_n_iterations()
```


## Value

number of iterations


## Examples

```r
get_n_iterations()
```


# `hedges_meta`

Estimated treatment effect and
 lower and upper confidence interval limits
 using Hedges method (Hedges & Olkin, 1985),
 implementation from package Meta.


## Description

For one experiment or study the individual
 effect is calculated, for more than one
 the overall effect is calculated


## Usage

```r
hedges_meta(
  sample_size,
  mean_e,
  sd_e,
  mean_c,
  sd_c,
  n_experiments,
  distribution = "normal",
  return = NULL
)
```


## Arguments

Argument      |Description
------------- |----------------
`sample_size`     |     Number of observations
`mean_e`     |     Estimated mean in experimental group
`sd_e`     |     Standard deviation in experimental group
`mean_c`     |     Estimated mean in control group
`sd_c`     |     Standard deviation in control group
`n_experiments`     |     Number of experiments
`distribution`     |     Distibution model
`return`     |     Return mode


## Value

Bounds, population effect or sample size based
 on parameter return


## Examples

```r
hedges_meta(10, 10, 3, 8, 3, 12, "population effect")
```


# `montecarlo`

Perform Monte Carlo simulations. See examples


## Description

Perform Monte Carlo simulations. See examples


## Usage

```r
montecarlo(
  matrix_comb,
  random_generation_function,
  metric_calculation_function
)
```


## Arguments

Argument      |Description
------------- |----------------
`matrix_comb`     |     Population parameters
`random_generation_function`     |     random generation function
`metric_calculation_function`     |     metric calculation function


## Value

Results of Monte Carlo simulations


## Examples

```r
# Example 1: Calculation of the value of pi

# Random process underlying the calculation of pi
# using the area method
# see, e.g., https://blogs.sas.com/content/iml/2016/03/14/monte-carlo-estimates-of-pi.html

fun0 <- function() {
x <- runif(metacomp::get_n_iterations())
y <- runif(metacomp::get_n_iterations())
ifelse(sqrt(x^2 + y^2) <= 1, 1, 0)
}

# Post-processing, in this case, the calculation of pi
# The results list is provided by the metacomp::montecarlo function
# results contains the output of fun0()

fun1 <- function(results) {
4 * sum(results[results == 1]) / metacomp::get_n_iterations()
}

# We set the number of samples to 1000
metacomp::set_n_iterations(1000)

# and make the calculation
metacomp::montecarlo(NULL, fun0, fun1)

# Please notice that metacomp::montecarlo() return a list of lists
# Each list element is the outcome of a different simulation
# This can be handy in some cases, e.g., see below.


# Example 2: Calculation of a definite integral


# Random process underlying the calculation of an integral
# see, e.g., https://blogs.sas.com/content/iml/2021/03/31/estimate-integral-monte-carlo.html

fun0 <- function(a, b, n) {
# This time we don't use the metacomp::get_n_iterations()
runif(n, a, b)
}

# Post-processing, where the area is calculated

fun1 <- function(a, b, results) {
# Take care of the order of the parameters because f receives 2
(b - a) * mean(as.numeric(lapply(results, f, a = 4)))
}

# The function to integrate should be defined somewhere

f <- function(x, a) {
x^(a - 1) * exp(-x)
}

# We can pass additional parameters to metacomp::montecarlo() using a matrix

m <- expand.grid(
a = 1,
b = 3.5,
n = c(1000, 10000, 100000)
)


# We make the calculation. Notice that the integral is calculated
# several times. Each row of m corresponds with a different calculation
metacomp::montecarlo(m, fun0, fun1)
```


# `montecarlo_meta`

Perform Monte Carlo simulations. See examples


## Description

Perform Monte Carlo simulations. See examples


## Usage

```r
montecarlo_meta(
  matrix_comb,
  random_generation_function,
  metric_calculation_function
)
```


## Arguments

Argument      |Description
------------- |----------------
`matrix_comb`     |     Population parameters
`random_generation_function`     |     random generation function
`metric_calculation_function`     |     metric calculation function


## Value

Results of Monte Carlo simulations


## Examples

```r
# Example 1: Calculation of the value of pi

# Random process underlying the calculation of pi
# using the area method
# see, e.g., https://blogs.sas.com/content/iml/2016/03/14/monte-carlo-estimates-of-pi.html

fun0 <- function() {
x <- runif(metacomp::get_n_iterations())
y <- runif(metacomp::get_n_iterations())
ifelse(sqrt(x^2 + y^2) <= 1, 1, 0)
}

# Post-processing, in this case, the calculation of pi
# The results list is provided by the metacomp::montecarlo function
# results contains the output of fun0()

fun1 <- function(results) {
4 * sum(results[results == 1]) / metacomp::get_n_iterations()
}

# We set the number of samples to 1000
metacomp::set_n_iterations(1000)

# and make the calculation
metacomp::montecarlo(NULL, fun0, fun1)

# Please notice that metacomp::montecarlo() return a list of lists
# Each list element is the outcome of a different simulation
# This can be handy in some cases, e.g., see below.


# Example 2: Calculation of a definite integral


# Random process underlying the calculation of an integral
# see, e.g., https://blogs.sas.com/content/iml/2021/03/31/estimate-integral-monte-carlo.html

fun0 <- function(a, b, n) {
# This time we don't use the metacomp::get_n_iterations()
runif(n, a, b)
}

# Post-processing, where the area is calculated

fun1 <- function(a, b, results) {
# Take care of the order of the parameters because f receives 2
(b - a) * mean(as.numeric(lapply(results, f, a = 4)))
}

# The function to integrate should be defined somewhere

f <- function(x, a) {
x^(a - 1) * exp(-x)
}

# We can pass additional parameters to metacomp::montecarlo() using a matrix

m <- expand.grid(
a = 1,
b = 3.5,
n = c(1000, 10000, 100000)
)


# We make the calculation. Notice that the integral is calculated
# several times. Each row of m corresponds with a different calculation
metacomp::montecarlo(m, fun0, fun1)
```


# `parametric_rr_meta`

Estimated treatment effect and
 lower and upper confidence interval limits
 using the Parametric Response Ratio method
 (Hedges et al, 1999), also known as
 Ratio of Means,
 implementation from package Meta.


## Description

For one experiment or study the individual
 effect is calculated, for more than one
 the overall effect is calculated


## Usage

```r
parametric_rr_meta(
  sample_size,
  mean_e,
  sd_e,
  mean_c,
  sd_c,
  n_experiments,
  distribution = "normal",
  return = NULL
)
```


## Arguments

Argument      |Description
------------- |----------------
`sample_size`     |     Number of observations
`mean_e`     |     Estimated mean in experimental group
`sd_e`     |     Standard deviation in experimental group
`mean_c`     |     Estimated mean in control group
`sd_c`     |     Standard deviation in control group
`n_experiments`     |     Number of experiments
`distribution`     |     Distibution model
`return`     |     Return mode


## Value

Bounds, population effect or sample size based
 on parameter return


## Examples

```r
hedges_meta(10, 10, 3, 8, 3, 12, "population effect")
```


# `plot_choose_method`

Plot metric equivalences between two methods


## Description

Plot metric equivalences between two methods


## Usage

```r
plot_choose_method(equivalences, ...)
```


## Arguments

Argument      |Description
------------- |----------------
`equivalences`     |     metric equivalences between two methods


## Value

a plot


# `plot_compare_methods`

Plot a comparison of methods


## Description

Plot a comparison of methods


## Usage

```r
plot_compare_methods(matrix_comb, metric, parameter, ...)
```


## Arguments

Argument      |Description
------------- |----------------
`matrix_comb`     |     methods and comparison information
`metric`     |     metric used in comparison to plot in vertical axis
`parameter`     |     parameter of interest to plot in horizontal axis


## Value

Graphical plot


# `rejection_rate`

Calculation of rejection rate for a list of estimated effects


## Description

Calculation of rejection rate for a list of estimated effects


## Usage

```r
rejection_rate(results, population_effect)
```


## Arguments

Argument      |Description
------------- |----------------
`results`     |     List of estimated effects bounds in format ( lower bound, upper bound )
`population_effect`     |     Effect of the population


## Value

Rate of rejection, in range 0-1


## Examples

```r
rejection_rate(list(list(c(1,3), c(-1, 4), c(1,4)), 3.2)
```


# `sample_generator`

lower and upper confidence interval limits


## Description

lower and upper confidence interval limits


## Usage

```r
sample_generator(
  n_experiments,
  sample_size,
  mean_e,
  sd_e,
  mean_c,
  sd_c,
  distribution
)
```


## Arguments

Argument      |Description
------------- |----------------
`n_experiments`     |     Number of experiments
`sample_size`     |     Number of observations
`mean_e`     |     Estimated mean in experimental group
`sd_e`     |     Standard deviation in experimental group
`mean_c`     |     Estimated mean in control group
`sd_c`     |     Standard deviation in control group
`distribution`     |     Distibution model


## Value

List of generated samples for
 experimental and control group


## Examples

```r
sample_generator(12, 50, 6, 3, 5, 3, "normal")
```


# `set_n_iterations`

Set the number of iterations the algorithm runs


## Description

Set the number of iterations the algorithm runs


## Usage

```r
set_n_iterations(val)
```


## Arguments

Argument      |Description
------------- |----------------
`val`     |     Number of iterations


## Examples

```r
set_n_iterations(200)
```




# quantrecipes

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis build
status](https://travis-ci.org/bagasbgy/quantrecipes.svg?branch=master)](https://travis-ci.org/bagasbgy/quantrecipes)
<!-- badges: end -->

## Installation

You can install the development version of `quantrecipes` using:

``` r
# install.packages("remotes")
remotes::install_github("bagasbgy/quantrecipes")
```

## Getting Started

Let’s start by importing the library:

``` r
# import libs
library(quantrecipes)
```

Here’s an example for setting-up the quantitative recipe steps:

``` r
# an example recipe
rec <- recipe(. ~ ., data = btcusdt) %>% 
  step_bbands(high, low, close, ma = "SMA", n = 20, sd_mult = 2) %>% 
  step_naomit(all_predictors()) %>% 
  prep()

# quick check
juice(rec)
#> # A tibble: 26,166 x 16
#>    datetime             open  high   low close volume turnover bbands_dn
#>    <dttm>              <dbl> <dbl> <dbl> <dbl>  <dbl>    <dbl>     <dbl>
#>  1 2019-05-01 02:05:00 5344. 5352. 5344  5352. 1.26      6716.     5329.
#>  2 2019-05-01 02:10:00 5353  5355. 5348. 5348. 0.191     1020.     5328.
#>  3 2019-05-01 02:15:00 5349. 5369. 5348. 5362. 0.310     1661.     5326.
#>  4 2019-05-01 02:20:00 5361. 5362. 5355. 5355. 0.356     1908.     5325.
#>  5 2019-05-01 02:25:00 5355. 5362. 5355. 5360. 0.860     4610.     5325.
#>  6 2019-05-01 02:30:00 5362. 5364. 5360. 5364. 0.0272     146.     5325.
#>  7 2019-05-01 02:35:00 5360. 5360. 5358. 5360. 0.263     1411.     5325.
#>  8 2019-05-01 02:40:00 5360. 5361. 5358. 5359. 1.03      5499.     5326.
#>  9 2019-05-01 02:45:00 5359. 5361. 5357. 5359. 0.515     2759.     5326.
#> 10 2019-05-01 02:50:00 5358. 5359. 5357. 5357. 1.17      6252.     5327.
#> # … with 26,156 more rows, and 8 more variables: bbands_ma <dbl>,
#> #   bbands_up <dbl>, bbands_pctb <dbl>, bbands_state <fct>,
#> #   bbands_state_count <int>, bbands_prev_state <fct>,
#> #   bbands_prev_range <fct>, bbands_prev_break <fct>
```

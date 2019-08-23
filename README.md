
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

Here’s a quick example for setting-up a recipe to produce Bollinger
Bands:

``` r
# an example recipe
rec <- recipe(. ~ ., data = btcusdt) %>% 
  step_bbands(high, low, close) %>% 
  step_naomit(all_predictors()) %>% 
  prep()

# quick check
juice(rec)
#> # A tibble: 26,172 x 11
#>    datetime             open  high   low close volume turnover bbands_dn
#>    <dttm>              <dbl> <dbl> <dbl> <dbl>  <dbl>    <dbl>     <dbl>
#>  1 2019-05-01 01:35:00 5340. 5340. 5331. 5331. 0.510     2722.     5322.
#>  2 2019-05-01 01:40:00 5338. 5339. 5333  5333  0.402     2146.     5324.
#>  3 2019-05-01 01:45:00 5335. 5337. 5333  5336. 0.819     4368.     5326.
#>  4 2019-05-01 01:50:00 5336. 5337. 5336. 5337. 0.0199     106.     5329.
#>  5 2019-05-01 01:55:00 5338. 5340. 5336. 5340. 1.39      7437.     5330.
#>  6 2019-05-01 02:00:00 5342. 5344. 5342  5344. 1.03      5504.     5330.
#>  7 2019-05-01 02:05:00 5344. 5352. 5344  5352. 1.26      6716.     5329.
#>  8 2019-05-01 02:10:00 5353  5355. 5348. 5348. 0.191     1020.     5328.
#>  9 2019-05-01 02:15:00 5349. 5369. 5348. 5362. 0.310     1661.     5326.
#> 10 2019-05-01 02:20:00 5361. 5362. 5355. 5355. 0.356     1908.     5325.
#> # … with 26,162 more rows, and 3 more variables: bbands_ma <dbl>,
#> #   bbands_up <dbl>, bbands_pctb <dbl>
```

Each steps also have `tidy()` methods if you need information regardings
the selected `"terms"`:

``` r
# steps[[1]] is our step_bbands
tidy(rec$steps[[1]])
#> # A tibble: 3 x 3
#>   terms value id          
#>   <chr> <chr> <chr>       
#> 1 h     high  bbands_YroXW
#> 2 l     low   bbands_YroXW
#> 3 c     close bbands_YroXW
```

or even the used `"params"`:

``` r
tidy(rec$steps[[1]], info = "params")
#> # A tibble: 1 x 8
#>   ma_fun     n sd_mult ma_options state prev_state state_options   id      
#>   <list> <dbl>   <dbl> <list>     <lgl> <lgl>      <list>          <chr>   
#> 1 <fn>      20       2 <list [0]> FALSE FALSE      <named list [4… bbands_…
```

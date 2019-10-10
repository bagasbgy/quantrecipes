# step interface ----------------------------------------------------------

#' @title Extract moving average features
#'
#' @description `step_ma` creates a **specification** of a recipe
#'  step that will extract **moving average** features from an asset price
#'  historical data.
#'
#' @param recipe A recipe object. The step will be added to the
#'  sequence of operations for this recipe.
#' @param ... One or more selector functions to choose which
#'  variables are affected by the step. See
#'  [selections (from recipes package)][recipes::selections] for more details.
#' @param ma_fun A `function` to extract moving average, or a `character`
#'  vector of length one which specify a moving average function.
#'  Defaults to [TTR::SMA].
#' @param n A `numeric` vector of length one which specify
#'  the moving average window.
#' @param weights A `character` vector of length one that specify a column name,
#'  or a `numeric` vector for `ma_fun` that has `wts` or `volume` argument.
#'  See details for more information.
#' @param ma_options A `list` of additional argument(s) that would be passed
#'  to `ma_fun` function.
#' @param state An option to specify whether to return
#'  the current states of the calculated moving averages. See details for
#'  more informations. Defaults to `FALSE`.
#' @param ratio Whether to return the moving average `spread` as ratio or
#'  absolute difference. See details for more informations. Defaults to `TRUE`.
#' @param prices A container for selected prices columns. Leave to `NULL`
#'  as it will be populated by [prep()][recipes::prep.recipe] function.
#' @param prefix A `character` vector of length one that would be used
#'  as a prefix to the created columns.
#' @param role For model terms created by this step, what analysis
#'  role should they be assigned? By default, the function assumes
#'  that the created columns will be used
#'  as `"predictors"` in a model.
#' @param trained A logical to indicate if the necessary informations for
#'  preprocessing have been estimated.
#' @param skip A logical. Should the step be skipped when the
#'  recipe is baked by [bake()][recipes::bake.recipe]? While all operations are baked
#'  when [prep()][recipes::prep.recipe] is run, some operations may not
#'  be able to be conducted on new data (e.g. processing
#'  the outcome variable(s)). Care should be taken when using `skip = TRUE`
#'  as it may affect the computations for subsequent operations
#' @param id A character string that is unique to this step to identify it.
#'
#' @return An updated version of `recipe` with the new step
#'  added to the sequence of existing steps (if any).
#'
#' @details
#'
#'  The output from this step are several new columns
#'  which contains the extracted moving average features.
#'
#'  For basic output, this step will produces:
#'
#'  * `value`: the moving average value
#'
#'  If `state` argument is `TRUE`, it will also produces:
#'
#'  * `spread`: current spread to the `prices` variable
#'  * `state`: current state of the spread; `"bullish"` or `"bearish"`
#'
#'  Note that if `ratio` argument is `TRUE`, the `spread` value would be
#'  returned as a ratio to `prices` variable instead of an absolute difference.
#'
#' @examples
#'
#' # import libs
#' library(quantrecipes)
#'
#' # basic usage
#' rec <- recipe(. ~ ., data = btcusdt) %>%
#'   step_ma(close) %>%
#'   step_naomit(all_predictors()) %>%
#'   prep()
#'
#' # get preprocessed data
#' juice(rec)
#'
#' # using state argument
#' rec <- recipe(. ~ ., data = btcusdt) %>%
#'   step_ma(close, state = TRUE) %>%
#'   step_naomit(all_predictors()) %>%
#'   prep()
#'
#' # get preprocessed data
#' juice(rec)
#'
#' # using pass-through options
#' rec <- recipe(. ~ ., data = btcusdt) %>%
#'   step_ma(close,
#'     ma_fun = TTR::EMA,
#'     n = 10,
#'     ma_options = list(wilder = TRUE),
#'     state = TRUE
#'   ) %>%
#'   step_naomit(all_predictors()) %>%
#'   prep()
#'
#' # get preprocessed data
#' juice(rec)
#'
#' # using custom weights for weighted moving average
#' rec <- recipe(. ~ ., data = btcusdt) %>%
#'   step_ma(close,
#'     ma_fun = TTR::WMA,
#'     n = 10,
#'     weights = c(rep(1, 9), 10),
#'     state = TRUE
#'   ) %>%
#'   step_naomit(all_predictors()) %>%
#'   prep()
#'
#' # get preprocessed data
#' juice(rec)
#'
#' # using volume-based moving average
#' rec <- recipe(. ~ ., data = btcusdt) %>%
#'   step_ma(close,
#'     ma_fun = TTR::VWMA,
#'     n = 10,
#'     weights = "volume",
#'     state = TRUE
#'   ) %>%
#'   step_naomit(all_predictors()) %>%
#'   prep()
#'
#' # get preprocessed data
#' juice(rec)
#'
#' @export

step_ma <- function(recipe, ..., ma_fun = TTR::SMA, n = 10, weights = NULL,
                    ma_options = list(), state = FALSE, ratio = TRUE,
                    prefix = "ma", prices = NULL,
                    role = "predictor", trained = FALSE,
                    skip = FALSE, id = rand_id("ma")) {

  # check selected terms
  terms <- ellipse_check(...)

  # add new step
  add_step(recipe, step_ma_new(
    terms = terms,
    ma_fun = ma_fun,
    n = n,
    weights = weights,
    ma_options = ma_options,
    state = state,
    ratio = ratio,
    prefix = prefix,
    prices = prices,
    role = role,
    trained = trained,
    skip = skip,
    id = id
  ))

}

step_ma_new <- function(terms, ma_fun, n, weights, ma_options, state, ratio,
                        prefix, prices, role, trained, skip, id) {

  # set-up step meta
  step("ma",
    terms = terms,
    ma_fun = ma_fun,
    n = n,
    weights = weights,
    ma_options = ma_options,
    state = state,
    ratio = ratio,
    prefix = prefix,
    prices = prices,
    role = role,
    trained = trained,
    skip = skip,
    id = id
  )

}

# prep and bake interface -------------------------------------------------

#' @export

prep.step_ma <- function(x, training, info = NULL, ...) {

  # get selected columns
  col_names <- terms_select(x$terms, info = info)

  # resolve selected columns
  x$prices <- col_names

  # check state options
  if (x$state == FALSE) {

    x$ratio <- FALSE

  }

  # prepare the step
  step_ma_new(
    terms = x$terms,
    ma_fun = x$ma_fun,
    n = x$n,
    weights = x$weights,
    ma_options = x$ma_options,
    state = x$state,
    ratio = x$ratio,
    prefix = x$prefix,
    prices = x$prices,
    role = x$role,
    trained = TRUE,
    skip = x$skip,
    id = x$id
  )


}

#' @export

bake.step_ma <- function(object, new_data, ...) {

  # create results containers
  ma_results <- NULL

  # get ma results
  for (i in object$prices) {

    # get selected terms as vectors
    price <- getElement(new_data, i)

    if (!is.null(object$weights)) {

      if (is.character(object$weights)) {

        weights <- getElement(new_data, object$weights)

      } else if (is.numeric(object$weights)) {

        weights <- object$weights

      }

    } else {

      weights <- object$weights

    }

    # list all args
    args_list <- list(
      price = price,
      ma_fun = object$ma_fun,
      n = object$n,
      weights = weights,
      ma_options = object$ma_options,
      state = object$state,
      ratio = object$ratio
    )

    # execute input-output function
    ma_result <- exec(get_ma, !!!args_list)

    # update column names
    col_names <- tolower(colnames(ma_result))
    col_names <- glue("{object$prefix}_{i}_{col_names}")

    colnames(ma_result) <- col_names

    # combine with container
    ma_results <- bind_cols(ma_results, ma_result)

  }

  # combine with prev data
  results <- bind_cols(new_data, ma_results)
  results <- as_tibble(results)

  # return the results
  results

}

# input-output resolver ---------------------------------------------------

# ma feature extractor
get_ma <- function(price, ma_fun, n, weights, ma_options, state, ratio) {

  # list all args
  if ("price" %in% names(formals(ma_fun))) {

    args_list <- list(
      price = price,
      n = n
    )

  } else {

    args_list <- list(
      x = price,
      n = n
    )

  }

  if (!is.null(weights)) {

    if ("wts" %in% names(formals(ma_fun))) {

      args_list <- c(args_list, list(wts = weights))

    } else if ("volume" %in% names(formals(ma_fun))) {

      args_list <- c(args_list, list(volume = weights))

    }

  }

  args_list <- c(args_list, ma_options)

  # calculate ma base features
  results <- exec(ma_fun, !!!args_list)

  # convert as tibble
  results <- tibble(
    price = price,
    value = as.numeric(results)
  )

  # record state
  if (state) {

    results <- results %>%
      mutate(spread = .data$price - .data$value)

    if (ratio) {

      results <- results %>%
        mutate(spread = .data$spread / .data$value)

    }

    results <- results %>%
      mutate(state = ifelse(.data$spread > 0, "bullish", "bearish"))

    # readjust all states to factor
    results <- results %>%
      mutate(state = factor(.data$state, levels = c("bullish", "bearish")))

  }

  # remove unnecessary cols
  results <- results %>%
    select(-.data$price)

  # return the results
  results

}

# tidy and print interface ------------------------------------------------

#' @rdname step_ma
#'
#' @param x A `step_ma` object.
#' @param info Options for `tidy()` method; whether to return tidied
#'  information for used `"terms"` or `"params"`
#'
#' @export

tidy.step_ma <- function(x, info = "terms", ...) {

  if (info == "terms") {

    if (is_trained(x)) {

      results <- tibble(prices = x$prices)

    } else {

      term_names <- sel2char(x$terms)

      results <- tibble(prices = term_names)

    }

  } else if (info == "params") {

    if (is(x$ma_fun, "character")) {

      x$ma_fun <- eval(parse_expr(x$ma_fun))

    }

    if (is.null(x$weights)) {

      x$weights <- NA

    } else if (is.numeric(x$weights)) {

      x$weights <- list(x$weights)

    }

    results <- tibble(
      ma_fun = list(x$ma_fun),
      n = x$n,
      weights = x$weights,
      ma_options = list(x$ma_options),
      state = x$state,
      ratio = x$ratio
    )

  }

  results$id <- x$id

  results

}

#' @export

print.step_ma <- function(x, width = max(20, options()$width - 29), ...) {

  msg <- glue("Extract Moving Averages features using: ")

  cat(msg, sep = "")
  printer(x$prices, x$terms, x$trained, width = width)

  invisible(x)

}

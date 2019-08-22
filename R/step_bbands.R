# step interface ----------------------------------------------------------

#' @title Extract Bollinger Bands features
#'
#' @description `step_bbands` creates a **specification** of a recipe
#'  step that will extract **Bollinger Bands** features
#'  from an asset price historical data.
#'
#' @param recipe A recipe object. The step will be added to the
#'  sequence of operations for this recipe.
#' @param ... Three (unquoted) column names, which, respectively,
#'  representing the `"high"`, `"low"`, and `"close"` prices.
#' @param ma A `function` to extract moving average elements, or a `character`
#'  vector of length one which specify a moving average function.
#'  Default to [TTR::SMA].
#' @param n A `numeric` vector of length one which specify
#'  moving average window. The default is `20`.
#' @param sd_mult A `numeric` vector of length one which specify
#'  standard deviation multiplier. The default is `2`.
#' @param h A container for the names of `"high"`. Leave to `NULL`
#'  as it will populated by [recipes::prep.recipe()] function.
#' @param l A container for the names of `"low"`. Leave to `NULL`
#'  as it will populated by [recipes::prep.recipe()] function.
#' @param c A container for the names of `"close"`. Leave to `NULL`
#'  as it will populated by [recipes::prep.recipe()] function.
#' @param prefix A `character` vector of length one that would be used
#'  as a prefix to the created bollinger bands columns. Default to `"bbands"`.
#' @param role For model terms created by this step, what analysis
#'  role should they be assigned? By default, the function assumes
#'  that the created bollinger bands columns will be used
#'  as predictors in a model.
#' @param trained A logical to indicate if the necessary informations for
#'  preprocessing have been estimated.
#' @param skip A logical. Should the step be skipped when the
#'  recipe is baked by [recipes::bake.recipe()]? While all operations are baked
#'  when [recipes::prep.recipe()] is run, some operations may not be able to be
#'  conducted on new data (e.g. processing the outcome variable(s)).
#'  Care should be taken when using `skip = TRUE` as it may affect
#'  the computations for subsequent operations
#' @param id A character string that is unique to this step to identify it.
#'
#' @return An updated version of `recipe` with the new step
#'  added to the sequence of existing steps (if any).
#'
#' @details
#'
#'  The output from this step are several new column
#'  which contains the extracted Bollinger Bands features:
#'
#'  * `dn`
#'  * `ma`
#'  * `up`
#'  * `pctb`
#'  * `state`
#'  * `state_count`
#'  * `prev_state`
#'  * `prev_range`
#'  * `prev_break`
#'
#' @examples
#'
#' # import libs
#' library(quantrecipes)
#'
#' # an example recipes using built-in data
#' rec <- recipe(. ~ ., data = btcusdt) %>%
#'   step_bbands(high, low, close, ma = "SMA", n = 20, sd_mult = 2) %>%
#'   step_naomit(all_predictors()) %>%
#'   prep()
#'
#' # get preprocessed data
#' juice(rec)
#'
#' @export

step_bbands <- function(recipe, ..., ma = TTR::SMA, n = 20, sd_mult = 2,
                        prefix = "bbands", h = NULL, l = NULL, c = NULL,
                        role = "predictor", trained = FALSE,
                        skip = FALSE, id = rand_id("bbands")) {

  # check selected terms
  terms <- ellipse_check(...)

  # add new step
  add_step(recipe, step_bbands_new(
    terms = terms,
    ma = ma,
    n = n,
    sd_mult = sd_mult,
    prefix = prefix,
    h = h,
    l = l,
    c = c,
    role = role,
    trained = trained,
    skip = skip,
    id = id
  ))

}

step_bbands_new <- function(terms, ma, n, sd_mult, prefix, h, l, c,
                            role, trained, skip, id) {

  # set-up step meta
  step("bbands",
    terms = terms,
    ma = ma,
    n = n,
    sd_mult = sd_mult,
    prefix = prefix,
    h = h,
    l = l,
    c = c,
    role = role,
    trained = trained,
    skip = skip,
    id = id
  )

}

# prep and bake interface -------------------------------------------------

#' @export

prep.step_bbands <- function(x, training, info = NULL, ...) {

  # get and resolve selected columns
  col_names <- terms_select(x$terms, info = info)

  x$h <- col_names[1]
  x$l <- col_names[2]
  x$c <- col_names[3]

  # prepare the step
  step_bbands_new(
    terms = x$terms,
    ma = x$ma,
    n = x$n,
    sd_mult = x$sd_mult,
    prefix = x$prefix,
    h = x$h,
    l = x$l,
    c = x$c,
    role = x$role,
    trained = TRUE,
    skip = x$skip,
    id = x$id
  )


}

#' @export

bake.step_bbands <- function(object, new_data, ...) {

  # extract prices as vector
  h <- getElement(new_data, object$h)
  l <- getElement(new_data, object$l)
  c <- getElement(new_data, object$c)

  # list all args
  args_list <- list(
    x = cbind(h, l, c),
    maType = object$ma,
    n = object$n,
    sd = object$sd_mult,
    high = 1,
    medhigh = 0.75,
    medlow = 0.25,
    low = 0

  )

  # execute input-output function
  bbands_results <- exec(get_bbands, !!!args_list)

  # update column names
  col_names <- tolower(colnames(bbands_results))
  col_names <- glue("{object$prefix}_{col_names}")

  colnames(bbands_results) <- col_names

  # combine with prev data
  results <- bind_cols(new_data, bbands_results)
  results <- as_tibble(results)

  # return the results
  results

}

# input-output resolver ---------------------------------------------------

# bbands feature extractor
get_bbands <- function(x, maType, n, sd, high, medhigh, medlow, low, ...) {

  # calculate bbands base features
  results <- BBands(
    HLC = x,
    maType = maType,
    n = n,
    sd = sd,
    ...
  )

  results <- as_tibble(results)

  colnames(results) <- c("dn", "ma", "up", "pctb")

  # record state
  results <- results %>%
    mutate(state = case_when(
      .data$pctb > high ~ "high",
      between(.data$pctb, medhigh, high) ~ "medhigh",
      between(.data$pctb, low, medlow) ~ "medlow",
      .data$pctb < low ~ "low",
      TRUE ~ "ranging"
    ))

  last_na <- n - 1

  results$state[1:last_na] <- NA

  results <- results %>%
    mutate(state_group = ifelse(.data$state != lag(.data$state), 1, 0)) %>%
    mutate(state_group = c(rep(NA, last_na), 1, .data$state_group[-c(1:(last_na + 1))])) %>%
    group_by(.data$state) %>%
    mutate(state_group = cumsum(.data$state_group)) %>%
    ungroup()

  results <- results %>%
    group_by(.data$state, .data$state_group) %>%
    mutate(state_count = row_number()) %>%
    ungroup() %>%
    select(-.data$state_group)

  results$state_count[1:last_na] <- NA

  # record prev state
  results <- results %>%
    mutate(prev_state = ifelse(.data$state != lag(.data$state), lag(.data$state), NA)) %>%
    fill(.data$prev_state, .direction = "down")

  # record prev break
  results <- results %>%
    mutate(prev_range = ifelse(.data$state %in% c("ranging", "medlow", "medhigh"), .data$state, NA)) %>%
    fill(.data$prev_range, .direction = "down")

  results <- results %>%
    mutate(prev_break = ifelse(.data$state %in% c("ranging", "medlow", "medhigh"), NA, .data$state)) %>%
    fill(.data$prev_break, .direction = "down")

  # return the results
  results

}

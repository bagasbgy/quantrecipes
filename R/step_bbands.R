# step interface ----------------------------------------------------------

#' @title Extract Bollinger Bands features
#'
#' @description `step_bbands` creates a **specification** of a recipe
#'  step that will extract **Bollinger Bands** features
#'  from an asset price historical data.
#'
#' @param recipe A recipe object. The step will be added to the
#'  sequence of operations for this recipe.
#' @param ... Either three or one (unquoted) column name(s). If three columns
#'  are given, it will represent the `"high"`, `"low"`, and `"close"` prices,
#'  respectively. Otherwise, if only one column name is given, it will treated
#'  as `"close"` price.
#' @param ma_fun A `function` to extract moving average elements, or a `character`
#'  vector of length one which specify a moving average function.
#'  Default to [TTR::SMA].
#' @param n A `numeric` vector of length one which specify
#'  moving average window. The default is `20`.
#' @param sd_mult A `numeric` vector of length one which specify
#'  standard deviation multiplier. The default is `2`.
#' @param ma_options A `list` of additional argument(s) that would be passed
#'  to `ma_fun` function.
#' @param state An option to specify whether to return
#'  the current states of the Bollinger Bands. Default to `TRUE`.
#' @param prev_state An option to specify whether to return
#'  the summary of previous states. Default to `TRUE` and
#'  only works if `state = TRUE`.
#' @param state_options A `list` of threshold that would be used
#'  as state determination. See details for further information.
#' @param h A container for the names of `"high"`. Leave to `NULL`
#'  as it will be populated by [recipes::prep.recipe()] function.
#' @param l A container for the names of `"low"`. Leave to `NULL`
#'  as it will be populated by [recipes::prep.recipe()] function.
#' @param c A container for the names of `"close"`. Leave to `NULL`
#'  as it will be populated by [recipes::prep.recipe()] function.
#' @param type A container for the final series type that
#'  would be used (`"hlc"` or `"c"`). Leave to `NULL` as it will be
#'  populated by [recipes::prep.recipe()] function.
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
#' @param info Options for `tidy()` method; whether to return tidied
#'  information for used `"terms"` or `"params"`
#'
#' @return An updated version of `recipe` with the new step
#'  added to the sequence of existing steps (if any).
#'
#' @details
#'
#'  The output from this step are several new column
#'  which contains the extracted Bollinger Bands features.
#'
#'  For basic output, this step will produces:
#'
#'  * `dn`: lower band
#'  * `ma`: moving average
#'  * `up`: upper band
#'  * `pctb`: calculated %B
#'
#'  If `state` argument is `TRUE`, it will also produces:
#'
#'  * `state`: current %B state
#'  * `state_count`: cumulative count in current state
#'
#'  These states are determined using four different threshold, which listed
#'  in `state_options` arguments. These are the default threshold values:
#'
#'  * `high`: `pctb > high` (the default is `high = 1`)
#'  * `medhigh`: `high > pctb > medhigh` (the default is `medhigh = 0.75`)
#'  * `medlow`: `low < pctb < medlow` (the default is `medlow = 0.25`)
#'  * `low`: `pctb < low` (the default is `low = 0`)
#'
#'  Note that the rest values would be categorized as `"medium"`.
#'
#'  Additionally, if `prev_state` argument is `TRUE`, it will also provides
#'  some summary regarding previous Bollinger Bands states:
#'
#'  * `prev_state`: previous state
#'  * `prev_medium`: previous medium-state
#'  * `prev_break`: previous break-state
#'
#' @examples
#'
#' # import libs
#' library(quantrecipes)
#'
#' # an example recipes using built-in data
#' rec <- recipe(. ~ ., data = btcusdt) %>%
#'   step_bbands(high, low, close,
#'     ma_fun = TTR::SMA,
#'     n = 20,
#'     sd_mult = 2
#'   ) %>%
#'   step_naomit(all_predictors()) %>%
#'   prep()
#'
#' # get preprocessed data
#' juice(rec)
#'
#' # using state argument
#' rec <- recipe(. ~ ., data = btcusdt) %>%
#'   step_bbands(high, low, close,
#'     ma_fun = TTR::SMA,
#'     n = 20,
#'     sd_mult = 2,
#'     state = TRUE
#'   ) %>%
#'   step_naomit(all_predictors()) %>%
#'   prep()
#'
#' # get preprocessed data
#' juice(rec)
#'
#' @export

step_bbands <- function(recipe, ..., ma_fun = TTR::SMA, n = 20, sd_mult = 2,
                        ma_options = list(), state = FALSE, prev_state = TRUE,
                        state_options = list(high = 1, medhigh = 0.75,
                                             medlow = 0.25, low = 0),
                        prefix = "bbands", h = NULL, l = NULL, c = NULL,
                        type = NULL, role = "predictor", trained = FALSE,
                        skip = FALSE, id = rand_id("bbands")) {

  # check selected terms
  terms <- ellipse_check(...)

  # add new step
  add_step(recipe, step_bbands_new(
    terms = terms,
    ma_fun = ma_fun,
    n = n,
    sd_mult = sd_mult,
    ma_options = ma_options,
    state = state,
    prev_state = prev_state,
    state_options = state_options,
    prefix = prefix,
    h = h,
    l = l,
    c = c,
    type = type,
    role = role,
    trained = trained,
    skip = skip,
    id = id
  ))

}

step_bbands_new <- function(terms, ma_fun, n, sd_mult, ma_options,
                            state, prev_state, state_options,
                            prefix, h, l, c, type,
                            role, trained, skip, id) {

  # set-up step meta
  step("bbands",
    terms = terms,
    ma_fun = ma_fun,
    n = n,
    sd_mult = sd_mult,
    ma_options = ma_options,
    state = state,
    prev_state = prev_state,
    state_options = state_options,
    prefix = prefix,
    h = h,
    l = l,
    c = c,
    type = type,
    role = role,
    trained = trained,
    skip = skip,
    id = id
  )

}

# prep and bake interface -------------------------------------------------

#' @export

prep.step_bbands <- function(x, training, info = NULL, ...) {
<<<<<<< HEAD

  # get selected columns
  col_names <- terms_select(x$terms, info = info)

  # resolve selected columns
  if (length(col_names) == 3) {

    x$h <- col_names[1]
    x$l <- col_names[2]
    x$c <- col_names[3]

    x$type <- "hlc"

  } else if (length(col_names) == 1) {

    x$h <- NA
    x$l <- NA
    x$c <- col_names[1]

    x$type <- "c"

  } else {

    stop(glue(
      "Invalid columns; please check the selected column(s): ",
      "{paste(col_names, collapse = ', ')}. ",
      "Are you mistakenly enter wrong argument? Please refer to ?step_bbands"
    ))

  }

  # check state options
  if (x$state == FALSE) {

    x$prev_state <- FALSE
=======

  # get selected columns
  col_names <- terms_select(x$terms, info = info)

  # resolve selected columns
  if (length(col_names) == 3) {

    x$h <- col_names[1]
    x$l <- col_names[2]
    x$c <- col_names[3]

    x$type <- "hlc"

  } else if (length(col_names) == 1) {

    x$h <- NA
    x$l <- NA
    x$c <- col_names[1]

    x$type <- "c"

  } else {

    stop(glue(
      "Invalid columns; please check the selected column(s): ",
      "{paste(col_names, collapse = ', ')}. ",
      "Are you mistakenly enter wrong argument? Please refer to ?step_bbands"
    ))
>>>>>>> 7427ea8e47de6372fba9bc6aba1f16689cf5dc0d

  }

  # prepare the step
  step_bbands_new(
    terms = x$terms,
    ma_fun = x$ma_fun,
    n = x$n,
    sd_mult = x$sd_mult,
    ma_options = x$ma_options,
    state = x$state,
    prev_state = x$prev_state,
    state_options = x$state_options,
    prefix = x$prefix,
    h = x$h,
    l = x$l,
    c = x$c,
    type = x$type,
    role = x$role,
    trained = TRUE,
    skip = x$skip,
    id = x$id
  )


}

#' @export

bake.step_bbands <- function(object, new_data, ...) {

  # extract prices as vector
  if (object$type == "hlc") {

    h <- getElement(new_data, object$h)
    l <- getElement(new_data, object$l)
    c <- getElement(new_data, object$c)

    x <- cbind(h, l, c)

  } else {

    x <- getElement(new_data, object$c)

  }

  # list all args
  args_list <- list(
    x = x,
    ma_fun = object$ma_fun,
    n = object$n,
    sd_mult = object$sd_mult,
    ma_options = object$ma_options,
    state = object$state,
    prev_state = object$prev_state,
    state_options = object$state_options
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
get_bbands <- function(x, ma_fun, n, sd_mult, ma_options,
                       state, prev_state, state_options) {

  # list all args
  args_list <- list(
    HLC = x,
    maType = ma_fun,
    n = n,
    sd = sd_mult
  )

  args_list <- c(args_list, ma_options)

  # calculate bbands base features
  results <- exec(BBands, !!!args_list)

  results <- as_tibble(results)

  colnames(results) <- c("dn", "ma", "up", "pctb")

  # record state
  if (state) {

    high <- state_options[["high"]]
    medhigh <- state_options[["medhigh"]]
    medlow <- state_options[["medlow"]]
    low <- state_options[["low"]]
<<<<<<< HEAD

    results <- results %>%
      mutate(state = case_when(
        .data$pctb > high ~ "high",
        between(.data$pctb, medhigh, high) ~ "medhigh",
        between(.data$pctb, low, medlow) ~ "medlow",
        .data$pctb < low ~ "low",
        TRUE ~ "medium"
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
    if (prev_state) {

      results <- results %>%
        mutate(prev_state = ifelse(.data$state != lag(.data$state), lag(.data$state), NA)) %>%
        fill(.data$prev_state, .direction = "down")

      results <- results %>%
=======

    results <- results %>%
      mutate(state = case_when(
        .data$pctb > high ~ "high",
        between(.data$pctb, medhigh, high) ~ "medhigh",
        between(.data$pctb, low, medlow) ~ "medlow",
        .data$pctb < low ~ "low",
        TRUE ~ "medium"
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
    if (prev_state) {

      results <- results %>%
        mutate(prev_state = ifelse(.data$state != lag(.data$state), lag(.data$state), NA)) %>%
        fill(.data$prev_state, .direction = "down")

      results <- results %>%
>>>>>>> 7427ea8e47de6372fba9bc6aba1f16689cf5dc0d
        mutate(prev_medium = ifelse(.data$state %in% c("medium", "medlow", "medhigh"), .data$state, NA)) %>%
        fill(.data$prev_medium, .direction = "down")

      results <- results %>%
        mutate(prev_break = ifelse(.data$state %in% c("medium", "medlow", "medhigh"), NA, .data$state)) %>%
        fill(.data$prev_break, .direction = "down")

    }

  }

  # return the results
  results

}

# tidy and print interface ------------------------------------------------

#' @rdname step_bbands
#' @param x A `step_bbands` object.
#' @export

tidy.step_bbands <- function(x, info = "terms", ...) {

  if (info == "terms") {

    if (is_trained(x)) {

      results <- tibble(
        terms = c("h", "l", "c"),
        value = c(x$h, x$l, x$c)
      )

    } else {

      term_names <- sel2char(x$terms)

      results <- tibble(
        terms = c("h", "l", "c"),
        value = na_chr
      )

    }

  } else if (info == "params") {

    if (is(x$ma_fun, "character")) {

      x$ma_fun <- eval(parse_expr(x$ma_fun))

    }

    results <- tibble(
      ma_fun = list(x$ma_fun),
      n = x$n,
      sd_mult = x$sd_mult,
      ma_options = list(x$ma_options),
      state = x$state,
      prev_state = x$prev_state,
      state_options = list(x$state_options),
    )

  }

  results$id <- x$id

  results

}

#' @export

print.step_bbands <- function(x, width = max(20, options()$width - 29), ...) {

  msg <- glue("Extract Bollinger Bands ({toupper(x$type)}) features using: ")

  cat(msg, sep = "")

  if (x$type == "hlc") {

    printer(c(x$h, x$l, x$c), x$terms, x$trained, width = width)

  } else if (x$type == "c") {

    printer(x$c, x$terms, x$trained, width = width)

  }

  invisible(x)

}

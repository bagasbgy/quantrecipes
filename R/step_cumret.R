# step interface ----------------------------------------------------------

#' @title Calculate cumulative returns based on specified actions
#'
#' @description `step_cumret` creates a **specification** of a recipe
#'  step that will calculate **cumulative returns** from a set of strategy
#'  actions and historical prices.
#'
#' @inheritParams step_ma
#'
#' @param recipe A recipe object. The step will be added to the
#'  sequence of operations for this recipe.
#' @param prices in development.
#' @param fee in development.
#' @param mult in development.
#' @param actions A container for selected actions columns. Leave to `NULL`
#'  as it will be populated by [prep()][recipes::prep.recipe] function.
#'
#' @inherit step_ma return
#'
#' @details
#'
#'  in development
#'
#' @examples
#'
#' # import libs
#' library(quantrecipes)
#'
#' # basic usage
#' rec <- recipe(. ~ ., data = actions) %>%
#'   step_cumret(benchmark, portfolio, prices = "close") %>%
#'   step_naomit(all_predictors()) %>%
#'   prep()
#'
#' # get preprocessed data
#' juice(rec)
#'
#' @export

step_cumret <- function(recipe, ..., prices, fee = 0.001, mult = FALSE,
                        prefix = "cumret", actions = NULL, role = "predictor",
                        trained = FALSE, skip = FALSE, id = rand_id("cumret")) {

  # check selected terms
  terms <- ellipse_check(...)

  # add new step
  add_step(recipe, step_cumret_new(
    terms = terms,
    prices = prices,
    fee = fee,
    mult = mult,
    prefix = prefix,
    actions = actions,
    role = role,
    trained = trained,
    skip = skip,
    id = id
  ))

}

step_cumret_new <- function(terms, prices, fee, mult, prefix, actions,
                            role, trained, skip, id) {

  # set-up step meta
  step("cumret",
    terms = terms,
    prices = prices,
    fee = fee,
    mult = mult,
    prefix = prefix,
    actions = actions,
    role = role,
    trained = trained,
    skip = skip,
    id = id
  )

}

# prep and bake interface -------------------------------------------------

#' @export

prep.step_cumret <- function(x, training, info = NULL, ...) {

  # get selected columns
  col_names <- terms_select(x$terms, info = info)

  # resolve selected columns
  x$actions <- col_names

  # prepare the step
  step_cumret_new(
    terms = x$terms,
    prices = x$prices,
    fee = x$fee,
    mult = x$mult,
    prefix = x$prefix,
    actions = x$actions,
    role = x$role,
    trained = TRUE,
    skip = x$skip,
    id = x$id
  )


}

#' @export

bake.step_cumret <- function(object, new_data, ...) {

  # create results containers
  cumret_results <- NULL

  # get ma results
  for (i in object$actions) {

    # get selected terms as vectors
    action <- getElement(new_data, i)
    price <- getElement(new_data, object$prices)

    # list all args
    args_list <- list(
      actions = action,
      prices = price,
      fee = object$fee,
      mult = object$mult
    )

    # execute input-output function
    cumret_result <- exec(get_cumret, !!!args_list)

    # update column names
    col_names <- tolower(colnames(cumret_result))
    col_names <- glue("{object$prefix}_{i}")

    colnames(cumret_result) <- col_names

    # combine with container
    cumret_results <- bind_cols(cumret_results, cumret_result)

  }

  # combine with prev data
  results <- bind_cols(new_data, cumret_results)
  results <- as_tibble(results)

  # return the results
  results

}

# input-output resolver ---------------------------------------------------

# cumulative return extractor
get_cumret <- function(actions, prices, fee, mult) {

  # combine all data
  results <- tibble(actions = actions, prices = prices)

  # readjust actions
  results <- results %>%
    mutate(actions = ifelse(.data$actions == "hold", NA, .data$actions)) %>%
    fill(.data$actions)

  if (is.na(first(results$actions))) {

    results[1, ] <- results[1, ] %>%
      mutate(actions = "buy")

    results <- results %>%
      fill(.data$actions)

  }

  # prepare fees information
  results <- results %>%
    mutate(fees = ifelse(.data$actions != lag(.data$actions), fee, 0))

  results[1, ] <- results[1, ] %>%
    mutate(fees = fee)

  # prepare returns
  results <- results %>%
    mutate(returns = lead(log(.data$prices)) - log(.data$prices))

  # calculate cumulative returns
  results <- results %>%
    mutate(results = ifelse(.data$actions == "buy", 1, -1) * .data$returns) %>%
    mutate(cumret = (1 + results) * (1 - .data$fees)) %>%
    mutate(cumret = cumprod(.data$cumret))

  # as asset multiplier or ROI
  if (!mult) {

    results <- results %>%
      mutate(cumret = .data$cumret - 1)

  }

  # readjust column
  results <- results %>%
    select(.data$cumret)

  # return the results
  results

}

# tidy and print interface ------------------------------------------------

#' @rdname step_cumret
#'
#' @param x A `step_cumret` object.
#' @param info Options for `tidy()` method; whether to return tidied
#'  information for used `"terms"` or `"params"`
#'
#' @export

tidy.step_cumret <- function(x, info = "terms", ...) {

  if (info == "terms") {

    if (is_trained(x)) {

      results <- tibble(actions = x$actions)

    } else {

      term_names <- sel2char(x$terms)

      results <- tibble(actions = term_names)

    }

  } else if (info == "params") {

    results <- tibble(
      prices = x$prices,
      fee = x$fee,
      mult = x$mult
    )

  }

  results$id <- x$id

  results

}

#' @export

print.step_cumret <- function(x, width = max(20, options()$width - 29), ...) {

  msg <- glue("Calculate cumulative returns using: ")

  cat(msg, sep = "")
  printer(x$actions, x$terms, x$trained, width = width)

  invisible(x)

}

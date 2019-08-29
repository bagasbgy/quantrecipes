# step interface ----------------------------------------------------------

#' @title Extract ZigZag features
#'
#' @description `step_zigzag` creates a **specification** of a recipe
#'  step that will extract **ZigZag** features from an asset price
#'  historical data.
#'
#' @inheritParams step_ma
#'
#' @param recipe A recipe object. The step will be added to the
#'  sequence of operations for this recipe.
#' @param ... Either three or one (unquoted) column name(s). If three columns
#'  are given, it will represent the `"high"`, and `"low"` prices, respectively.
#'  Otherwise, if only one column name is given, it will treated
#'  as `"close"` price.
#' @param change A `numeric` vector of length one, to specify the minimum price
#'  change to be considered as the change from peak or trough.
#' @param percent A `logical` to specify whether the `change` argument should
#'  be interpreted as percentage change (`TRUE`) or absolute change (`FALSE`)
#' @param retrace A `logical` to specify whether the `change` argument should
#'  be interpreted as a change from previous move, or from last peak/trough.
#' @param state An option to specify whether to return
#'  the current states of the ZigZag. Defaults to `FALSE`.
#' @param span A `numeric` vector of length one to specify the `swing` span.
#' Default to `c(0, 0)`; zero addition to backward and forward. See details.
#' @param h A container for the names of `"high"`. Leave to `NULL`
#'  as it will be populated by [prep()][recipes::prep.recipe] function.
#' @param l A container for the names of `"low"`. Leave to `NULL`
#'  as it will be populated by [prep()][recipes::prep.recipe] function.
#' @param c A container for the names of `"close"`. Leave to `NULL`
#'  as it will be populated by [prep()][recipes::prep.recipe] function.
#' @param type A container for the final series type that
#'  would be used (`"hl"` or `"c"`). Leave to `NULL` as it will be
#'  populated by [prep()][recipes::prep.recipe] function.
#'
#' @inherit step_ma return
#'
#' @details
#'
#'  The output from this step are several new columns
#'  which contains the extracted moving average features.
#'
#'  For basic output, this step will produces:
#'
#'  * `value`: the estimated ZigZag value
#'
#'  If `state` argument is `TRUE`, it will also produces:
#'
#'  * `trend`: current trend
#'  * `swing`: swing points; either `"up"`, `"down"`, or `"hold"`
#'
#'  Note that the `"up"` and `"down"` are determined as
#'  the first `trend` change; you can control its wide and position using
#'  `span` arguments.
#'
#' @examples
#'
#' # import libs
#' library(quantrecipes)
#'
#' # basic usage
#' rec <- recipe(. ~ ., data = btcusdt) %>%
#'   step_zigzag(close) %>%
#'   step_naomit(all_predictors()) %>%
#'   prep()
#'
#' # get preprocessed data
#' juice(rec)
#'
#' @export

step_zigzag <- function(recipe, ..., change = 1, percent = TRUE,
                        retrace = FALSE, state = FALSE, span = c(0, 0),
                        prefix = "zigzag", h = NULL, l = NULL, c = NULL,
                        type = NULL, role = "predictor", trained = FALSE,
                        skip = FALSE, id = rand_id("zigzag")) {

  # check selected terms
  terms <- ellipse_check(...)

  # add new step
  add_step(recipe, step_zigzag_new(
    terms = terms,
    change = change,
    percent = percent,
    retrace = retrace,
    state = state,
    span = span,
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

step_zigzag_new <- function(terms, change, percent, retrace, state, span,
                            prefix, h, l, c, type, role, trained, skip, id) {

  # set-up step meta
  step("zigzag",
    terms = terms,
    change = change,
    percent = percent,
    retrace = retrace,
    state = state,
    span = span,
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

prep.step_zigzag <- function(x, training, info = NULL, ...) {

  # get selected columns
  col_names <- terms_select(x$terms, info = info)

  # resolve selected columns
  if (length(col_names) == 2) {

    x$h <- col_names[1]
    x$l <- col_names[2]
    x$c <- NA

    x$type <- "hl"

  } else if (length(col_names) == 1) {

    x$h <- NA
    x$l <- NA
    x$c <- col_names[1]

    x$type <- "c"

  } else {

    stop(glue(
      "Invalid columns; please check the selected column(s): ",
      "{paste(col_names, collapse = ', ')}. ",
      "Are you mistakenly enter wrong argument? Please refer to ?step_zigzag"
    ))

  }

  # prepare the step
  step_zigzag_new(
    terms = x$terms,
    change = x$change,
    percent = x$percent,
    retrace = x$retrace,
    state = x$state,
    span = x$span,
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

bake.step_zigzag <- function(object, new_data, ...) {

  # extract prices as vector
  if (object$type == "hl") {

    h <- getElement(new_data, object$h)
    l <- getElement(new_data, object$l)

    x <- cbind(h, l)

  } else {

    x <- getElement(new_data, object$c)

  }

  # list all args
  args_list <- list(
    x = x,
    change = object$change,
    percent = object$percent,
    retrace = object$retrace,
    state = object$state,
    span = object$span
  )

  # execute input-output function
  zigzag_results <- exec(get_zigzag, !!!args_list)

  # update column names
  col_names <- tolower(colnames(zigzag_results))
  col_names <- glue("{object$prefix}_{col_names}")

  colnames(zigzag_results) <- col_names

  # combine with prev data
  results <- bind_cols(new_data, zigzag_results)
  results <- as_tibble(results)

  # return the results
  results

}

# input-output resolver ---------------------------------------------------

# zigzag feature extractor
get_zigzag <- function(x, change, percent, retrace, state, span) {

  # list all args
  args_list <- list(
    HL = x,
    change = change,
    percent = percent,
    retrace = retrace,
    lastExtreme = TRUE
  )

  # calculate zigzag base features
  results <- exec(ZigZag, !!!args_list)

  # convert as tibble
  results <- tibble(value = results)

  if (state) {

    # add trend
    results <- results %>%
      mutate(trend = ifelse(.data$value > lag(.data$value), "up", "down"))

    # add swing according to first trend change
    results <- results %>%
      mutate(swing = ifelse(.data$trend != lag(.data$trend), "swing", "hold"))

    results <- results %>%
      mutate(swing = case_when(
        .data$swing == "swing" & .data$trend == "up" ~ "up",
        .data$swing == "swing" & .data$trend == "down" ~ "down",
        TRUE ~ .data$swing
      ))

    # readjust swing span and position
    if (span[1] > 0) {

      for (i in 1:span[1]) {

        results <- results %>%
          mutate(swing = ifelse(lead(.data$swing) != "hold", lead(.data$swing), .data$swing))

      }

    }

    if (span[2] > 0) {

      for (i in 1:span[2]) {

        results <- results %>%
          mutate(swing = ifelse(lag(.data$swing) != "hold", lag(.data$swing), .data$swing))

      }

    }

    if (span[1] < 0) {

      for (i in 1:-span[1]) {

        results <- results %>%
          mutate(swing = ifelse(lag(.data$swing) == "hold", lag(.data$swing), .data$swing))

      }

    }


    if (span[2] < 0) {

      for (i in 1:-span[2]) {

        results <- results %>%
          mutate(swing = ifelse(lead(.data$swing) == "hold", lead(.data$swing), .data$swing))

      }

    }

  }

  # return the results
  results

}

# tidy and print interface ------------------------------------------------

#' @rdname step_zigzag
#'
#' @param x A `step_zigzag` object.
#' @param info Options for `tidy()` method; whether to return tidied
#'  information for used `"terms"` or `"params"`
#'
#' @export

tidy.step_zigzag <- function(x, info = "terms", ...) {

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

    results <- tibble(
      change = x$change,
      percent = x$percent,
      retrace = x$retrace,
      state = x$state,
      span = list(x$span)
    )

  }

  results$id <- x$id

  results

}

#' @export

print.step_zigzag <- function(x, width = max(20, options()$width - 29), ...) {

  msg <- glue("Extract ZigZag ({toupper(x$type)}) features using: ")

  cat(msg, sep = "")

  if (x$type == "hlc") {

    printer(c(x$h, x$l, x$c), x$terms, x$trained, width = width)

  } else if (x$type == "c") {

    printer(x$c, x$terms, x$trained, width = width)

  }

  invisible(x)

}

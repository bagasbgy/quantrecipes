# example dataset ---------------------------------------------------------

#' @title Example trading actions
#'
#' @details
#'
#'  This dataset provide trading actions data, which
#'  created using [btcusdt][btcusdt] data and [step_zigzag()][step_zigzag]
#'  for an example trading actions.
#'
#' @source the close prices are retrieved from [KuCoin](https://kucoin.com)
#'  market's API.
#'
#' @format An object of class `tibble`.
#'
#' @usage data(actions)
#'
#' @examples
#'
#' # import library
#' library(quantrecipes)
#'
#' # print the data
#' actions
#'
#' @docType data

"actions"

# script used to acquire the data -----------------------------------------

# # import libs
# library(quantrecipes)
#
# # get zigzag
# rec <- recipe(. ~ ., data = tail(btcusdt, 12 * 24 * 3)) %>%
#   step_zigzag(close, change = 0.5, state = TRUE, span = c(1, -1)) %>%
#   prep()
#
# # create example actions data
# actions <- juice(rec) %>%
#   mutate(portfolio = case_when(
#     zigzag_swing == "up" ~ "buy",
#     zigzag_swing == "down" ~ "sell",
#     TRUE ~ "hold"
#   )) %>%
#   mutate(portfolio = c("buy", .data$portfolio[-1])) %>%
#   mutate(benchmark = "hold") %>%
#   mutate(benchmark = c("buy", .data$benchmark[-1])) %>%
#   select(datetime, close, benchmark, portfolio)
#
# # save the data
# usethis::use_data(actions, overwrite = TRUE)

# example dataset ---------------------------------------------------------

#' @title BTC/USDT historical data
#'
#' @details
#'
#'  This historical data is obtained from [KuCoin](https://kucoin.com)
#'  market's API, which queried using
#'  [`rucoin`](https://github.com/bagasbgy/rucoin). The dataset is ranging
#'  from May 1, 2019 to July 31, 2019, and contains following informations:
#'
#'  * `datetime`
#'  * `open`
#'  * `high`
#'  * `low`
#'  * `close`
#'  * `volume`
#'  * `turnover`
#'
#' @source [KuCoin](https://kucoin.com) market's API.
#'
#' @format An object of class `tibble`.
#'
#' @usage data(btcusdt)
#'
#' @examples
#'
#' # import library
#' library(quantrecipes)
#'
#' # print the data
#' btcusdt
#'
#' @docType data

"btcusdt"

# script used to acquire the data -----------------------------------------

# # import libs
# library(lubridate)
# library(rucoin)
# library(usethis)
#
# # set date range
# from <- ymd("2019-05-01")
# to <- ymd("2019-07-31")
#
# # get data
# btcusdt <- get_kucoin_prices(
#   symbols = "BTC/USDT",
#   from = from,
#   to = to,
#   frequency = "5 minutes"
# )
#
# # save the data
# usethis::use_data(btcusdt, overwrite = TRUE)

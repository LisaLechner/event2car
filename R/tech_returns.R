#' Stock returns of 18 US tech companies
#'
#' Datset containing stock returns of 18 important US tech firms (See \href{https://finance.yahoo.com/u/yahoo-finance/watchlists/tech-stocks-that-move-the-market/}{Yahoo Finance})
#' and the NASDAQ return.
#' The dataset covers all trading days between 2015-11-03 and 2017-11-29 (e.g. 523).
#'
#' @docType data
#'
#' @usage data("tech_returns", package="estudy2car")
#'
#' @keywords datasets
#'
#' @format An objects of class \code{zoo} containing 503 observations and 19 variables.
#' \describe{
#'   \item{^NDX}{NASDAQ return  from 2015-11-03 to 2017-11-29.}
#'   \item{MSFT}{Microsoft's from 2015-11-03 to 2017-11-29.}
#'   \item{AMZ}{Amazon's from 2015-11-03 to 2017-11-29.}
#'   \item{AAPL}{Apple's from 2015-11-03 to 2017-11-29.}
#'   \item{GOOG}{Google's from 2015-11-03 to 2017-11-29.}
#'   \item{FB}{Facebook's from 2015-11-03 to 2017-11-29.}
#'   \item{BABA}{Alibaba's from 2015-11-03 to 2017-11-29.}
#'   \item{INTC}{Intel's from 2015-11-03 to 2017-11-29.}
#'   \item{PYPL}{PayPal's from 2015-11-03 to 2017-11-29.}
#'   \item{NVDA}{NVIDIA's from 2015-11-03 to 2017-11-29.}
#'   \item{TSLA}{Tesla's from 2015-11-03 to 2017-11-29.}
#'   \item{ATVI}{Activision Blizzard's from 2015-11-03 to 2017-11-29.}
#'   \item{AMD}{Advanced Micro's from 2015-11-03 to 2017-11-29.}
#'   \item{EA}{Electronic Arts's from 2015-11-03 to 2017-11-29.}
#'   \item{MTCH}{Match Group's from 2015-11-03 to 2017-11-29.}
#'   \item{TTD}{The Trade Desk's from 2015-11-03 to 2017-11-29.}
#'   \item{ZG}{Zillow Group's from 2015-11-03 to 2017-11-29.}
#'   \item{YELP}{Yelp's from 2015-11-03 to 2017-11-29.}
#'   \item{TIVO}{TiVo's from 2015-11-03 to 2017-11-29.}
#'   ...
#' }
#' @source \href{https://finance.yahoo.com/}{Yahoo Finance}
#'
#' @examples
#' data('tech_returns')
#' # prepare data
#' trumpelection <- as.Date("2016-11-08")
#' returns_firms=tech_returns[,2:19]
#' return_indx = tech_returns[,1]
#' # mean adjusted model
#' event2car(returns=returns_firms,regressor=return_indx,
#'           event_dates=trumpelection,market_model="mean_adj")
#' # market adjusted model (out-of sample estimation)
#' event2car(returns=returns_firms,regressor=return_indx,
#'           event_dates=trumpelection,market_model="mrkt_adj")
#' # market adjusted model (within sample estimation)
#' event2car(returns=returns_firms,regressor=return_indx,
#'           event_dates=trumpelection,market_model="mrkt_adj_within")
"tech_returns"


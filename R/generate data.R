library(estudy2)
tickers <- c("^NDX","MSFT","AMZ","AAPL","GOOG","FB","BABA","INTC","PYPL",
"NVDA","TSLA","ATVI","AMD","EA","MTCH","TTD","ZG","YELP","TIVO")

prices <- get_prices_from_tickers(tickers,
                                  start = as.Date("2015-11-01"),
                                  end = as.Date("2017-11-30"),
                                  quote = "Close",
                                  retclass = "zoo")
rates <- get_rates_from_prices(prices,
                               quote = "Close",
                               multi_day = TRUE,
                               compounding = "continuous")

returns <- rates

saveRDS(returns,"./data/tech_returns.rds")



library(estudy2)
tickers <- c("^NDX","MSFT","AMZ","AAPL","GOOG","FB","BABA","INTC","PYPL",
             "NVDA","TSLA","ATVI","AMD","EA","MTCH","TTD","ZG","YELP","TIVO")

prices <- get_prices_from_tickers(tickers,
                                  start = as.Date("2015-11-01"),
                                  end = as.Date("2019-12-30"),
                                  quote = "Close",
                                  retclass = "zoo")
rates <- get_rates_from_prices(prices,
                               quote = "Close",
                               multi_day = TRUE,
                               compounding = "continuous")

returns <- rates

saveRDS(returns,"./data/tech_returns.rds")

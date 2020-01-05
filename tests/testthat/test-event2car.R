context("event2car")
library(event2car)

test_that("Length of list equals number of event dates", {
  regressor <- as.matrix(rnorm(400))
  returns <- cbind(rnorm(400),rnorm(400),rnorm(400),rnorm(400),rnorm(400),rnorm(400))
  dates <- seq(as.Date("2016-01-01"), length = 400, by = "days")

  regressor <- zoo::zoo(regressor)
  returns <- zoo::zoo(returns)

  zoo::index(regressor) <- dates
  zoo::index(returns) <- dates

  event_date <- "2016-08-03"
  output <- event2car(returns = returns,
            imputation_returns = "mean",
            event_date = event_date,method="mean_adj",
            car_lag = 1, car_lead = 5, estimation_period = 150)
  expect_equal(length(output),length(event_date))


  event_date <- c("2016-08-03","2016-12-01")
  output <- event2car(returns = returns, regressor = regressor,
                      imputation_returns = "mean",
                      imputation_regressor = "mean",
                      event_date = event_date, method="mrkt_adj_within",
                      car_lag = 1, car_lead = 5, estimation_period = 150)
  expect_equal(length(output),length(event_date))

})

test_that("Check correct NA handling.", {
  regressor <- cbind(rnorm(400),rnorm(400))
  nas <- sample(length(regressor), 100)
  regressor[nas] <- NA
  returns <- cbind(rnorm(400))
  nas <- sample(length(returns), 30)
  returns[nas] <- NA
  dates <- seq(as.Date("2016-01-01"), length = 400, by = "days")

  regressor <- zoo::zoo(regressor)
  returns <- zoo::zoo(returns)

  zoo::index(regressor) <- dates
  zoo::index(returns) <- dates

  event_date <- "2016-08-03"

  expect_error(event2car(returns = returns,
                         imputation_returns = "pmm",
                         event_date = event_date, method="mean_adj",
                         car_lag = 1, car_lead = 5, estimation_period = 150))

  expect_warning(event2car(returns = returns,
                           regressor = regressor,
                           imputation_returns = "approx",
                           imputation_regressor = "approx",
                           event_date = event_date, method="mrkt_adj_within",
                           car_lag = 1, car_lead = 5, estimation_period = 150))

  expect_error(event2car(returns = returns,
                           regressor = regressor,
                           imputation_returns = "drop",
                           imputation_regressor = "approx",
                           event_date = event_date,method="mrkt_adj_within",
                           car_lag = 1, car_lead = 5, estimation_period = 150))

})

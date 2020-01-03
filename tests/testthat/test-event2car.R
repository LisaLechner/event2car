context("event2car")
library(event2car)



test_that("Length of list equals number of event dates", {
  regressor <- as.matrix(rnorm(400))
  returns <- cbind(rnorm(400),rnorm(400),rnorm(400),rnorm(400),rnorm(400),rnorm(400))
  dates <- seq(as.Date("2016-01-01"), length = 400, by = "days")

  regressor <- zoo(regressor)
  returns <- zoo(returns)

  zoo::index(regressor) <- dates
  zoo::index(returns) <- dates

  event_date <- "2016-08-03"
  output <- event2car(returns = returns,
            imputation = "mean",
            event_date = event_date,method="mean_adj",
            car_lag = 1,car_lead = 5,estimation_period = 150)
  expect_equal(length(output),length(event_date))


  event_date <- c("2016-08-03","2016-12-01")
  output <- event2car(returns = returns, regressor = regressor,
                      imputation = "mean",
                      event_date = event_date,method="mrkt_adj_within",
                      car_lag = 1,car_lead = 5,estimation_period = 150)
  expect_equal(length(output),length(event_date))

})

#' event2car: Calculates \code{CAR} (cumulative abnormal returns) for firm(s) and event(s)
#'
#' The function applies the following steps to securities' rates of returns: A)
#' calculate abnormal returns for \code{estimation_period}(s), B) predict
#' abnormal returns for event period(s), C) aggregate the predicted abnormal
#' returns to cumulative abnormal return(s) for \code{event_dates}.
#' The package covers three models for the calculation of the cumulative abnormal returns:
#' - Mean-Adjusted Model (\code{mean_adj})
#' - Market-Adjusted Model (\emph{mrkt_adj})
#' - Within-Sample Marekt-Adjusted Model (\emph{mrkt_adj_within})
#' This is the logic suggested by multiple scholars. See references below.
#'
#' The generic function is dispatched for such classes as
#'  \code{zoo}. (future versions of the package allow for classes of \code{data.frame}.)
#' If \code{market_model} is \emph{mrkt_adj} or \code{sim}
#' and \code{regressor} has the length greater than one, the first element of
#' \code{regressor} will be applied for each security in \code{returns}.
#'
#' @param returns an object of \code{data.frame} or \code{zoo} containing rates of returns of securities.
#' @param regressor an object of the same class as \code{returns} containing regressors.
#'                  The argument can be omitted, if market model is \code{mean_adj}.
#' @param event_dates an object of class \code{Date} containing one event date or multiple event dates
#' @param estimation_period an object of class \code{intenger} stating the number of days
#'  prior to the event over which the market model parameters are estimated. Default is 250 days.
#'  Note that the event period itself is not included in the event period to prevent the event from influencing the normal performance model parameter estimates.
#' @param car_lag an object of class \code{intenger} measuring the start of the event window. The default is 1 day prior to the event date.
#' @param car_lead an object of class \code{intenger} measuring the end of the event window. The default is 5 days after the event date.
#' @param market_model market_model a character indicating the market model among
#' \code{mean_adj}, \code{mrkt_adj}, and \code{sim}.

#' @return an object of class \code{data.frame} which contains cumulative abnormal returns, the average cumulative abnormal return
#' (controls for varying event period durations if non-trading days are in the period), the number of tradingdays,
#' significance levels of the market-return-coefficient, significance level of the event-dummy-coefficient, and model fit (rsquared) per securities per event date(s).
#' Note that significance levels and rsquared are NA if market model is \code{mean_adj}.
#'
#' @references MacKinlay, A.C. \emph{Event Studies in Economics and Finance}.
#' Journal of Economic Literature, 35(1):13-39, 1997.
#' @references Brown S.J., Warner J.B. \emph{Using Daily Stock Returns, The Case
#' of Event Studies}. Journal of Financial Economics, 14:3-31, 1985.
#' @references Davies, R., Studnicka, Z. \emph{The heterogeneous impact of Brexit: Early indications from the FTSE}.
#' European Economic Review, 110:1-17, 2018.
#' @importFrom plyr ldply
#' @importFrom zoo index merge.zoo coredata
#' @importFrom stats na.omit time
#'
#' @keywords eventstudy stock finance
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


#' @export
event2car <- function(returns=returns,
                      regressor=regressor,
                      event_dates = event_dates,
                      estimation_period= 250,
                      car_lag = 0,
                      car_lead = 5,
                      market_model=c("mean_adj", "mrkt_adj","mrkt_adj_within")){

  market_model<- match.arg(market_model)

  car_event <- lapply(seq_along(event_dates),function(ev){

    estimation_start<-event_dates[ev]-car_lag-estimation_period
    estimation_end<-event_dates[ev]-car_lag
    event_start<-event_dates[ev]-car_lag
    event_end<-event_dates[ev]+car_lead

    car <- lapply(1:ncol(as.data.frame(returns)), function(firm){
      return <- returns[,firm]
      if(market_model %in% c("mean_adj")){
        data <- return}
      if(market_model %in% c("mrkt_adj","mrkt_adj_within")){
        data <- merge.zoo(return, regressor,  all = TRUE)}
      estimation_data <- data[stats::complete.cases(data), ]

      if(market_model %in% c("mrkt_adj","mean_adj")){
        estimation_data <- estimation_data[
          zoo::index(estimation_data) >= estimation_start &
            zoo::index(estimation_data) <= estimation_end]}

      if(market_model %in% c("mrkt_adj_within")){
        estimation_data <- estimation_data[
          zoo::index(estimation_data) >= estimation_start &
            zoo::index(estimation_data) <= event_end] }

      prediction_data <- data[stats::complete.cases(data),]
      prediction_data <- prediction_data[
        zoo::index(prediction_data) >= event_start &
          zoo::index(prediction_data) <= event_end]

      if(length(estimation_data)>0 & length(prediction_data)>0){
        if(market_model =="mean_adj"){
          estimation_mean <- mean(estimation_data,na.rm=T)
          abnormal <- prediction_data[, 1] - estimation_mean
          tradingdays <- table(!is.na(abnormal))[["TRUE"]]
          car <- data.frame(car=sum(zoo::zoo(abnormal[,1])),car_ave = sum(zoo::zoo(abnormal[,1]))/tradingdays,tradingdays=tradingdays)
        }

        if(market_model %in% c("mrkt_adj")){

          y <- zoo::coredata(estimation_data[, 1])
          x <- zoo::coredata(estimation_data[, 2])

          lm_fit <- stats::lm(y ~ x)
          if(!is.na(lm_fit$coefficients[2])){
            predicted <- stats::predict.lm(object = lm_fit,
                                           newdata = data.frame(x = prediction_data[, 2]),
                                           interval = c("confidence"), level = 0.95)
            abnormal <-  prediction_data[, 1] - predicted[, 1]
            tradingdays <- table(!is.na(abnormal))[["TRUE"]]
            car <- data.frame(car=sum(zoo::zoo(abnormal[,1])),car_ave =sum(zoo::zoo(abnormal[,1]))/tradingdays,tradingdays=tradingdays ,sign_mrkt=summary(lm_fit)$coefficients[2,4],r=summary(lm_fit)$r.squared)
          } else{
            warning('NaN created for CAR: No data during event window or estimation window.')
            car <- data.frame(car=NaN,car_ave=NA,tradingdays=NA,sign_mrkt=NaN,r=NaN)
          }
        }

        if(market_model %in% c("mrkt_adj_within")){
          estimation_data$event = ifelse(index(estimation_data) %in% event_start:event_end,1,0)
          if(length(table(estimation_data$event))>1){
            y <- zoo::coredata(estimation_data[, 1])
            x <- zoo::coredata(estimation_data[, 2])
            z <- zoo::coredata(estimation_data[, 3])
            lm_fit <- stats::lm(y ~ x+z)
            tradingdays <- nrow(na.omit(subset(subset(estimation_data,time(estimation_data) %in% event_start:event_end))))
            car <- data.frame(car=lm_fit$coefficients[3],car_ave=lm_fit$coefficients[3],tradingdays=tradingdays,sign_mrkt=summary(lm_fit)$coefficients[2,4],sign_car=summary(lm_fit)$coefficients[3,4],r=summary(lm_fit)$r.squared)
          }
          else{
            warning('NaN created for CAR: Not enough data in estimation and/or prediction data.')
            car <- data.frame(car=NaN,car_ave=NA,tradingdays=NA,sign_mrkt=NaN,sign_car=NaN,r=NaN)
          }
        }
      }
      else{

        if(market_model %in% c("mrkt_adj")){
          warning('NaN created for CAR: No data during event window or estimation window.')
          car <- data.frame(car=NaN,car_ave=NaN,tradingdays=NaN,sign_mrkt=NaN,r=NaN)
        }
        if(market_model %in% c("mrkt_adj_within")){
          warning('NaN created for CAR: No data during event window or estimation window.')
          car <- data.frame(car=NaN,car_ave=NaN,tradingdays=NaN,sign_mrkt=NaN,sign_car=NaN,r=NaN)
        }
        if(market_model %in% c("mean_adj")){
          warning('NaN created for CAR: No data during event window or estimation window.')
          car <- data.frame(car=NaN,car_ave=NaN,tradingdays=NaN)
        }

      }
      return(car)})
    car <- ldply(car)
    car$firm <- names(returns)
    return(car)

  })
  names(car_event) <- event_dates
  car_event <- ldply(car_event)

  if(market_model %in% c("mrkt_adj")){
    names(car_event) <- c("event","car","car_ave","tradingdays","sign_mrkt","rsquared","firm")
  }
  if(market_model %in% c("mrkt_adj_within")){
    names(car_event) <- c("event","car","car_ave","tradingdays","sign_mrkt","sign_car","rsquared","firm")
  }
  if(market_model %in% c("mean_adj")){
    names(car_event) <- c("event","car","car_ave","tradingdays","firm")
  }

  return(car_event)
}



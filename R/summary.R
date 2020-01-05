#' Summarizing cumulative abnormal returns
#'
#' summary method for the class "event2car_range".
#'
#' @param object an object of class \code{event2car_range}.
#' @keywords internal
#' @method summary event2car_range
#' @importFrom stats reshape qnorm sd
#' @examples
#' trumpelection <- as.Date("2016-11-08")
#' returns_firms <- tech_returns[,2:19]
#' return_indx <- tech_returns[,1]
#' effect_trump <- event2car_range(returns=returns_firms,regressor=return_indx,
#'                                 imputation_returns="mean",
#'                                 event_date=trumpelection,method="mean_adj")
#' summary(effect_trump)
#' @export
summary.event2car_range <- function(object){
  if (!class(object) == "event2car_range"){
    stop("Summary works for event2car_range objects only.")
  }

  if (class(object$car_timeseries) == "zoo") {
    use <- data.frame(object$car_timeseries)
    use$date <- row.names(use)
    use <- stats::reshape(use, idvar="date",
                          varying = 1:(ncol(use)-1),
                          v.names = "car",
                          direction="long")
    row.names(use) <- NULL
    names(use) <- c("date","firm","car")
    use$date <- as.Date(use$date)
  }
  if (class(object$car_timeseries) == "data.frame") {
    use <- object$car_timeseries
    row.names(use) <- NULL
  }
  use$firm <- as.character(use$firm)

  event_dates_out <- paste(length(object$event_date),"event dates:",paste(as.character(object$event_date),collapse=", "),"\n \n")
  firms_out <- paste(length(object$firm),"firms:",paste(as.character(object$firm),collapse=", "))
  use2 <- use[use$date %in% object$event_date,]
  car_mean_out <- aggregate(use2$car,list(use2$date),FUN=function(y) mean(y,na.rm=TRUE))
  names(car_mean_out) <- c("event_date","mean_car")
  car_sd_out <- aggregate(use2$car,list(use2$date),FUN=function(y) stats::sd(y,na.rm=TRUE))
  names(car_sd_out) <- c("event_date","sd_car")
  car_summary_out <- merge(car_mean_out,car_sd_out)
  car_summary_out$ci_lower <- car_summary_out$mean_car - car_summary_out$sd_car * qnorm(0.975) / sqrt(length(use$firm))
  car_summary_out$ci_upper <- car_summary_out$mean_car + car_summary_out$sd_car * qnorm(0.975) / sqrt(length(use$firm))
  car_summary_out$sd_car <- NULL

  cat("**** Summary of event2car_range object ****\n\n",event_dates_out,firms_out,"\n\n")
  return(car_summary_out)
}

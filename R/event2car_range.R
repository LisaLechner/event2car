#' Cumulative abnormal returns for firm(s) at event(s)
#'
#' The function calculates cumulative abnormal returns
#' for a specific time-range before and after the event(s).
#'
#' The package covers three models for the calculation of the cumulative abnormal returns:
#' - Mean-adjusted model (\code{mean_adj})
#' - Market-adjusted model using out-of-sample estimation (\emph{mrkt_adj_out})
#' - Market-adjusted model using within-sample estimation (\emph{mrkt_adj_within})
#' This is the logic suggested by multiple scholars. See references below.
#'
#' The generic function is dispatched for such classes as
#'  \code{zoo}. (future versions of the package allow for classes of \code{data.frame}.)
#'
#' If \code{method} is \emph{mrkt_adj_out} or \code{mrkt_adj_within}
#' and \code{regressor} has the length greater than one, the first element of
#' \code{regressor} will be applied for each security in \code{returns}.
#'
#' @param returns an object of class \code{zoo} containing rates of returns of securities.
#' @param regressor an object of the same class as \code{returns} containing regressors.
#'                  The argument can be omitted, if market model is \code{mean_adj}.
#' @param event_date a character object or an object of class \code{Date} containing one event date or multiple event dates in the format YYYY-MM-DD.
#' @param estimation_period an object of class \code{intenger} stating the number of days
#'  prior to the event over which the market model parameters are estimated. Default is 150 days.
#'  Note that the event period itself is not included in the event period to prevent the event from influencing the normal performance model parameter estimates.
#' @param car_lag an object of class \code{intenger} measuring the start of the event window. The default is 1 day prior to the event date.
#' @param car_lead an object of class \code{intenger} measuring the end of the event window. The default is 5 days after the event date.
#' @param method a character indicating the method used to calculate abnormal returns and cumulative abnormal returns. Choose among
#' \code{mean_adj}, \code{mrkt_adj_out}, and \code{mrkt_adj_within}.
#' @param imputation_returns a character indicating the way of dealing with missing values in returns data:
#' \code{drop}: No imputation at all and dropping observations with NAs,
#' \code{mean}: imputing missing data with the mean stock return (for further information see `?zoo::na.aggregate`) ,
#' \code{approx}: imputing missing data using interpolated values (for further information see `?zoo::na.approx`),
#' \code{pmm}: imputing missing data by dint of predictive mean matching used in the mice package (for further information see `?mice::mice`).
#'
#' @param imputation_regressor a character indicating the way of dealing with missing values in regressor data:
#' \code{mean}: imputing missing data with the mean stock return (for further information see `?zoo::na.aggregate`) ,
#' \code{approx}: imputing missing data using interpolated values (for further information see `?zoo::na.approx`),
#' @param range_before a numeric value indicating the time range to be included before the first event.
#' @param range_after a numeric value indicating the time range to be included after the last event.
#' @param output_format a character value signaling whether the cumulative abnormal returns
#' time series should be returned as \code{zoo} or \code{data.frame} object.
#'
#' @return an object of class \code{event2car_range} which contains the
#' cumulative abnormal returns time series (\code{car_timeseries}),
#' information on \code{event_dates},
#' information on \code{firms},
#' and information on \code{method}, \code{imputation_returns}, and \code{imputation_regressor}.
#'
#' @references MacKinlay, A.C. \emph{Event Studies in Economics and Finance}.
#' Journal of Economic Literature, 35(1):13-39, 1997.
#' @references Brown S.J., Warner J.B. \emph{Using Daily Stock Returns, The Case
#' of Event Studies}. Journal of Financial Economics, 14:3-31, 1985.
#' @references Davies, R., Studnicka, Z. \emph{The heterogeneous impact of Brexit: Early indications from the FTSE}.
#' European Economic Review, 110:1-17, 2018.
#' @importFrom zoo index na.aggregate zoo
#' @importFrom stats coef confint qnorm sd time window lm
#'
#' @keywords eventstudy stock finance timeseries
#'
#' @examples
#' trumpelection <- as.Date("2016-11-08")
#' returns_firms <- tech_returns[,2:19]
#' return_indx <- tech_returns[,1]
#' effect_trump <- event2car_range(returns=returns_firms,regressor=return_indx,
#'                                 imputation_returns="mean",
#'                                 event_date=trumpelection,method="mean_adj")
#' @export
event2car_range <- function(returns = NULL, regressor = NULL, event_date = NULL,
                      method = c("mean_adj","mrkt_adj_within","mrkt_adj_out"),
                      imputation_returns = c("approx","mean","drop","pmm"),
                      imputation_regressor = c("approx","mean"),
                      car_lag = 1,car_lead = 5,estimation_period = 150,
                      range_before = 30, range_after = 30,
                      output_format = c("zoo","data.frame")){

  event_date <- as.Date(event_date)
  range <- seq((min(as.Date(event_date))-range_before), (max(as.Date(event_date))+range_after), "days")

  # Sanity Checks
  method <- match.arg(method)
  imputation_returns <- match.arg(imputation_returns)
  imputation_regressor <- match.arg(imputation_regressor)
  output_format <- match.arg(output_format)

  if (NROW(returns) < estimation_period+car_lag+car_lead) {
    stop("Length of returns is too short.
                     Either use a longer returns object or shorten the estimation- or event period.")
  }
  if (min(stats::time(returns))>=min(event_date)-estimation_period-car_lag | max(stats::time(returns))<=max(event_date)-car_lead){
    stop("Length of returns is too short.
                     Either use a longer returns object or shorten the estimation- or event period.")
  }

  if (method %in% c("mrkt_adj_within","mrkt_adj_out")){
    if (!is.null(ncol(regressor))) {
      regressor <- regressor[ ,1]
    }



    if (NROW(returns) != NROW(regressor)) {
      stop("Length of returns is not equal to length of regressor")
    }
    if (NROW(regressor) < estimation_period+car_lag+car_lead) {
      stop("Length of regressor is too short.
                         Either use a longer returns object or shorten the estimation- or event period.")
    }
    if (min(stats::time(regressor))>=min(event_date)-estimation_period-car_lag | max(stats::time(regressor))<=max(event_date)-car_lead){
      stop("Length of regressor is too short.
                     Either use a longer regressor object or shorten the estimation- or event period.")
    }
    if (any(!stats::time(regressor) %in% stats::time(regressor))) {
      warning("regressor and returns have different time ranges.")
    }
  }


  # Handling missing data of returns
  ## mean imputation_returns
  if (imputation_returns == "mean") {
    if (any(colSums(is.na(returns))>0)) {
      missing <- colnames(returns)[colSums(is.na(returns))>0]
      warning(paste("Imputed data using mean returns of the following firm(s):",
                    paste(missing, collapse=' ')))
    }
    returns <- zoo::na.aggregate(returns,FUN=mean,na.rm=T)
  }
  ## mean imputation_returns
  if (imputation_returns == "approx") {
    if (any(colSums(is.na(returns))>0)) {
      missing <- colnames(returns)[colSums(is.na(returns))>0]
      warning(paste("Replace NA values of returns data with interpolated values of the following firm(s):",
                    paste(missing, collapse=' ')))
    }
    returns <- zoo::na.approx(returns)
    if (any(colSums(is.na(returns))>0)) {
      missing <- colnames(returns)[colSums(is.na(returns))>0]
      warning(paste("Drop following firm(s) due to too many missing returns data (unable to fill by dint of interpolated values):",
                    paste(missing, collapse=' ')))
    }
    returns <- returns[,colSums(is.na(returns))==0]
  }
  ## no imputation_returns: dropping firms with NAs
  if (imputation_returns == "drop") {
    if (any(colSums(is.na(returns))>0)) {
      missing <- colnames(returns)[colSums(is.na(returns))>0]
      warning(paste("Drop following firm(s) due to missing returns data:",
                    paste(missing, collapse=' ')))
    }
    if (any(ncol(returns)<2|is.null(ncol(returns))) & sum(is.na(returns))>0) {
      stop("All firms dropped due to missing data. No data to estimate cumulative abnormal returns.")
    } else {
      returns <- returns[,colSums(is.na(returns))==0]
    }
  }
  ## pmm: predictive mean matching
  if (imputation_returns == "pmm") {
    if (ncol(returns)==1) {
      stop("Predictive mean matching requires at least two firms in the data.")
    }
    if (requireNamespace("mice", quietly = TRUE)) {
      if (any(colSums(is.na(returns))>0)) {
        missing <- colnames(returns)[colSums(is.na(returns))>0]
        warning(paste("Impute returns data using predictive mean matching of the following firm(s):",
                      paste(missing, collapse=' ')))
      }


      dates_returns <- stats::time(returns)
      returns <- mice::complete(mice::mice(returns,method="pmm",printFlag = FALSE))
      row.names(returns) <- dates_returns
      returns <- zoo::zoo(returns)
      zoo::index(returns) <- dates_returns
    } else {
      stop("'mice' package not installed. Choose another imputation method for returns object or run install.packages('mice')")
    }
  }

  if (any(is.na(returns)|is.nan(returns)|is.infinite(returns))) {
    stop("Returns data has NA/NaN/Inf value(s) and no imputation has been choosen.")
  }

  # Handling missing data of regressor
  if (method %in% c("mrkt_adj_within","mrkt_adj_out")){
    ## mean imputation_regressor
    if (imputation_regressor == "mean") {
      if (sum(is.na(regressor))>0) {
        warning("Impute data using mean regressor.")
      }
      regressor <- zoo::na.aggregate(regressor,FUN=mean,na.rm=T)
    }

    ## mean imputation_regressor
    if (imputation_regressor == "approx") {
      if (sum(is.na(regressor))>0) {
        warning("Replace NA values in regressor with interpolated values.")
      }
      regressor <- zoo::na.approx(regressor,na.rm=T)
    }

    if (any(is.na(regressor)|is.nan(regressor)|is.infinite(regressor))) {
      stop("Regressor has NA/NaN/Inf value(s) and no imputation has been choosen.")
    }
  }

temp <- lapply(range,function(event){
  ## Mean adjusted model
  if (method == "mean_adj") {
    ### data prep
    #### estimation period data
    y <- window(returns, start=(event-estimation_period-car_lag),end=(event-car_lag))
    #### event period data
    y2 <- window(returns, start=(event-car_lag),end=(event+car_lead))
    ### abnormal returns
    if (any(is.null(ncol(y)),ncol(y)==1)) {
      car <- sum(as.numeric(y2) - mean(as.numeric(y)))
      car_lb <- car - sd(as.numeric(y2) - mean(as.numeric(y)))/sqrt(length(y2))
      car_ub <- car + sd(as.numeric(y2) - mean(as.numeric(y)))/sqrt(length(y2))
    } else {
    ar <- sapply(seq_len(ncol(y)),function(i){
      as.numeric(y2[,i]) - mean(as.numeric(y[,i]))})

    car <- colSums(ar)

    car_lb <- sapply(seq_len(ncol(y)),function(i){
      car[i] - sd(as.numeric(y2[,i]) - mean(as.numeric(y[,i])))/sqrt(length(y2[,i])) })

    car_ub <- sapply(seq_len(ncol(y)),function(i){
      car[i] + sd(as.numeric(y2[,i]) - mean(as.numeric(y[,i])))/sqrt(length(y2[,i])) })



    }
  }
  ## Market adjusted (within sample) model
  if (method == "mrkt_adj_within") {
    ### data prep
    y <- window(returns, start=(event-estimation_period-car_lag),end=(event+car_lead))
    x1 <- window(regressor, start=(event-estimation_period-car_lag),end=(event+car_lead))
    x2 <- stats::time(y) %in% seq(as.Date(event-car_lag), as.Date(event+car_lead), "days")
    ### regression inkl. event period
    z <- stats::lm(y~x1+x2)
    ### output
    if (any(is.null(ncol(y)),ncol(y)==1)) {
      car <- stats::coef(z)[[3]]* sum(ifelse(x2==TRUE,1,0))
      car_lb <- NA # need to implement this
      car_ub <- NA # need to implement this
    } else {
      car <- stats::coef(z)[3,] * sum(ifelse(x2==TRUE,1,0))
      car_lb <- NA # need to implement this
      car_ub <- NA # need to implement this
    }
  }
  ## Market adjusted (within sample) model
  if (method == "mrkt_adj_out") {
    ### data prep
    #### estimation period data
    y <- stats::window(returns, start=(event-estimation_period-car_lag),end=(event-car_lag))
    x1 <- stats::window(regressor, start=(event-estimation_period-car_lag),end=(event-car_lag))
    #### event period data
    y2 <- stats::window(returns, start=(event-car_lag),end=(event+car_lead))
    x12 <- stats::window(regressor, start=(event-car_lag),end=(event+car_lead))
    ### regression
    z <- stats::lm(y~x1)
    ### prediction or abnormal returns

    if (any(is.null(ncol(y)),ncol(y)==1)) {

      ar <- sapply(seq_along(x12),function(i){
        y2[[i]] - (stats::coef(z)[[1]] + stats::coef(z)[[2]] * x12[[i]])
      })

      car <- sum(ar)
      car_lb <- NA # need to implement this
      car_ub <- NA # need to implement this

    } else {
      ar <- sapply(seq_along(x12),function(i){
        y2[i, ] - (stats::coef(z)[1, ] + stats::coef(z)[2, ] * x12[[i]])
      })
    car <- rowSums(ar)
    car_lb <- NA # need to implement this
    car_ub <- NA # need to implement this
    }
  }
 car <- data.frame(firm=names(y),car=car,car_lb=car_lb,car_ub=car_ub)


  if (is.null(names(y))) {
    car <- data.frame(firm=seq_len(length(y)),car=car,car_lb=car_lb,car_ub=car_ub)
  }
  return(car)
})

#temp <- unlist(temp)
#temp <- data.frame(firm=names(temp),car=temp)
temp <- temp[order(temp$firm),]
if (nrow(temp) %% length(range) ==0) {
  temp$date <- rep(range,nrow(temp)/length(range))
} else {
  stop("Unbalanced dataset.")
}

if (output_format=="zoo") {
  temp2 <- stats::reshape(temp,timevar ="firm",idvar="date",
                          direction="wide")
  names(temp2) <- gsub("car.","",names(temp2))
  temp3 <- zoo::zoo(temp2[2:ncol(temp2)])
  zoo::index(temp3) <- temp2[[1]]
}

if (output_format=="data.frame") {
 temp3 <- temp
}

out <- list(temp3,event_date,as.character(unique(temp$firm)),method,imputation_returns,imputation_regressor)
names(out) <- c("car_timeseries","event_date","firm","method","imputation_returns","imputation_regressor")
class(out) <- "event2car_range"
return(out)
}









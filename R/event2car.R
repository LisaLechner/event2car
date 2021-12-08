#' Cumulative abnormal returns for firm(s) at event(s)
#'
#' The function calculates abnormal returns and confidence intervals
#' during the event period(s) as well as
#' cumulative abnormal returns of event(s).
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
#' @param event_dates a character object or an object of class \code{Date} containing one event date or multiple event dates in the format YYYY-MM-DD.
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
#'@param method_nontradingdays a character indicating how to treat non-trading days during eventperiod.
#'The method \code{add} adds the amount of non-trading days before or as the case may be
#'after the eventperiod. The method \code{skip} skips non-trading days from the eventperiod, but does not add additonal days.
#'The latter results in unequal eventperiods across \code{event_dates}.
#' @param imputation_regressor a character indicating the way of dealing with missing values in regressor data:
#' \code{mean}: imputing missing data with the mean stock return (for further information see `?zoo::na.aggregate`) ,
#' \code{approx}: imputing missing data using interpolated values (for further information see `?zoo::na.approx`),
#'
#' @return an object of class \code{list} which contains abnormal returns on the event date(s),
#' confidence intervals of the abnormal returns, and the cumulative abnormal return of the event period(s).
#' Note that the \code{mean_adj} adn the \code{mrkt_adj_out} method produce mean abnormal returns,
#' yet the \code{mrkt_adj_within} method delivers coefficients of the event period.
#'
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
#' @keywords eventstudy stock finance
#'
#' @examples
#' # prepare data
#' trumpelection <- as.Date("2016-11-08")
#' returns_firms <- tech_returns[,2:19]
#' return_indx <- tech_returns[,1]
#' # mean adjusted model
#' event2car(returns=returns_firms,regressor=return_indx,
#'          imputation_returns="mean",
#'          event_dates=trumpelection,method="mean_adj")
#' # market adjusted model (out-of sample estimation)
#' event2car(returns=returns_firms,regressor=return_indx,
#'          imputation_returns="mean",imputation_regressor="approx",
#'          event_dates=trumpelection,method="mrkt_adj_out")
#' # market adjusted model (within sample estimation)
#' event2car(returns=returns_firms,regressor=return_indx,
#'         imputation_returns="mean",imputation_regressor="approx",
#'         event_dates=trumpelection,method="mrkt_adj_within")

#' @export
event2car <- function(returns = NULL,regressor = NULL,event_dates = NULL,
                      method = c("mean_adj","mrkt_adj_within","mrkt_adj_out"),
                      imputation_returns = c("approx","mean","drop","pmm"),
                      imputation_regressor = c("approx","mean"),
                      method_nontradingdays = c("add","skip"),
                      car_lag = 1,car_lead = 5,estimation_period = 150){


  # Sanity Checks
  method <- match.arg(method)
  method_nontradingdays <- match.arg(method_nontradingdays)
  imputation_returns <- match.arg(imputation_returns)
  imputation_regressor <- match.arg(imputation_regressor)

  event_dates <- as.Date(event_dates)

  # check non-trading days
  if(method_nontradingdays=="add"){
  if(exists(regressor)){
  if(unique(weekdays(index(returns)))!=unique(weekdays(index(regressor)))){
    warning("Non-trading days differ in returns and regressor data. Information from the returns data is used to determine event period.")
  }}}

  # check dimensions
  if (NROW(returns) < estimation_period+car_lag+car_lead) {
    stop("Length of returns is too short.
                     Either use a longer returns object or shorten the estimation- or event period.")
  }
  if (min(stats::time(returns))>=min(event_dates)-estimation_period-car_lag | max(stats::time(returns))<=max(event_dates)-car_lead){
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
    if (min(stats::time(regressor))>=min(event_dates)-estimation_period-car_lag | max(stats::time(regressor))<=max(event_dates)-car_lead){
      stop("Length of regressor is too short.
                     Either use a longer regressor object or shorten the estimation- or event period.")
    }
    if (any(!stats::time(regressor) %in% stats::time(regressor))) {
      warning("regressor and returns have different time ranges.")
    }
  }

  # define non-trading days
  if(method_nontradingdays=="add"){
  nontradingdays <-c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday") %in% unique(weekdays(index(returns)))
  nontradingdays <- c(nontradingdays,nontradingdays) # to allow for a sequence of two weeks
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

  # Estimate abnormal returns, confidence intervals,
  # and the cumulative abnormal return for each event and each firm
  temp <- lapply(event_dates,function(event){
    eventperiod_start <- event-car_lag
    eventperiod_end <- event+car_lead

    if(method_nontradingdays == "add"){
      eventday <-  which(c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday") %in% weekdays(event))
      if(nontradingdays[eventday-1]==FALSE & nontradingdays[eventday-2]==TRUE){
        eventperiod_start <- eventperiod_start-1}
      if(nontradingdays[eventday-1]==FALSE & nontradingdays[eventday-2]==FALSE){
        eventperiod_start <- eventperiod_start-2}
      if(sum(nontradingdays[(eventday+2):(eventday+6)])<2){
        eventperiod_end <- eventperiod_end+(5-sum(nontradingdays[(eventday+1):(eventday+5)]))}
      if(sum(nontradingdays[(eventday+2):(eventday+6)])==2){
        eventperiod_end <- eventperiod_end+3}
    }

    ## Mean adjusted model
    if (method == "mean_adj") {
      ### data prep
      #### estimation period data
      y <- window(returns, start=(event-estimation_period-car_lag),end=(event-car_lag))
      #### event period data
      y2 <- window(returns, start=eventperiod_start,end=eventperiod_end)
      ### abnormal returns
      if (any(is.null(ncol(y)),ncol(y)==1,nrow(y),nrow(y2))) {
        ar <- as.numeric(y2) - mean(as.numeric(y))
        se <- stats::qnorm(0.975)*stats::sd(ar)/sqrt(length(ar))
        m <- mean(ar)
        ci <- c(m-se, m+se)
        out <- data.frame(m,t(ci),sum(ar))
        names(out) <- c("ar_mean","ci_lower","ci_upper","car")
      } else {
      ar <- sapply(seq_len(ncol(y)),function(i){
        as.numeric(y2[,i]) - mean(as.numeric(y[,i]))})

      ci <- sapply(seq_len(ncol(ar)),function(i){
        se <- stats::qnorm(0.975)*stats::sd(ar[,i])/sqrt(nrow(ar))
        m <- mean(ar[,i])
        c(m-se, m+se)
      })
      ci <- t(ci)
      ### output
      out <- cbind(colMeans(ar),ci)
      colnames(out) <- c("ar_mean","ci_lower","ci_upper")
      out <- data.frame(out)
      out$car <- colSums(ar)
      }

      out$firm <- names(y)
      row.names(out) <- NULL
      if (is.null(out$firm)) {
        out$firm <- 1:nrow(out)
      }
      out <- out[c(5,1:4)]
    }
    ## Market adjusted (within sample) model
    if (method == "mrkt_adj_within") {
      ### data prep
      y <- window(returns, start=(event-estimation_period-car_lag),end=eventperiod_end)
      x1 <- window(regressor, start=(event-estimation_period-car_lag),end=eventperiod_end)
      x2 <- stats::time(y) %in% seq(as.Date(eventperiod_start),as.Date(eventperiod_end), "days")
      ### regression inkl. event period
      z <- stats::lm(y~x1+x2)
      ### output
      if (any(is.null(ncol(y)),ncol(y)==1)) {
        out <- c(stats::coef(z)[[3]],stats::confint(z)[c(FALSE,FALSE,TRUE),])
        names(out) <- NULL
        out <- t(out)
      } else {
        out <- cbind(stats::coef(z)[3,],
                     stats::confint(z)[rep(c(FALSE,FALSE,TRUE),ncol(y)),])
      }
      colnames(out) <- c("ar_coef","ci_lower","ci_upper")

      out <- data.frame(out)
      out$car <- out$ar * sum(ifelse(x2==TRUE,1,0))
      out$firm <- rownames(out)
      if ("X.x2TRUE" %in% out$firm) {
        out$firm <- 1:nrow(out)
      }
      row.names(out) <- NULL

      out <- out[c(5,1:4)]
    }
    ## Market adjusted (within sample) model
    if (method == "mrkt_adj_out") {
      ### data prep
      #### estimation period data
      y <- stats::window(returns, start=(event-estimation_period-car_lag),end=(event-car_lag))
      x1 <- stats::window(regressor, start=(event-estimation_period-car_lag),end=(event-car_lag))
      #### event period data
      y2 <- stats::window(returns, start=eventperiod_start,end=eventperiod_end)
      x12 <- stats::window(regressor, start=eventperiod_start,end=eventperiod_end)
      ### regression
      z <- stats::lm(y~x1)
      ### prediction or abnormal returns

      if (any(is.null(ncol(y)),ncol(y)==1)) {

        ar <- sapply(seq_along(x12),function(i){
          y2[[i]] - (stats::coef(z)[[1]] + stats::coef(z)[[2]] * x12[[i]])
        })

        ar_se <- qnorm(0.975)*sd(ar)/sqrt(length(ar))
        ar_mean <- mean(ar)
        ci <- c(ar_mean-ar_se, ar_mean+ar_se)
        ci <- t(ci)
        out <- cbind(ar_mean,ci)
        colnames(out) <- c("ar_mean","ci_lower","ci_upper")
        out <- data.frame(out)
        out$car <- sum(ar)

      } else {
        ar <- sapply(seq_along(x12),function(i){
          y2[i, ] - (stats::coef(z)[1, ] + stats::coef(z)[2, ] * x12[[i]])
        })
        ci <- sapply(seq_len(nrow(ar)),function(i){
          ar_se <- qnorm(0.975)*sd(ar[i,])/sqrt(ncol(ar))
          ar_mean <- mean(ar[i,])
          c(ar_mean-ar_se, ar_mean+ar_se)
        })
        ci <- t(ci)

        ### output
        out <- cbind(rowMeans(ar),ci)
        colnames(out) <- c("ar_mean","ci_lower","ci_upper")
        out <- data.frame(out)
        out$car <- rowSums(ar)
      }

      out$firm <- rownames(out)
      if (is.null(out$firm)) {
        out$firm <- 1:nrow(out)
      }
      row.names(out) <- NULL

      out <- out[c(5,1:4)]
    }
    return(out)
  })
  names(temp) <- event_dates
  return(temp)
}



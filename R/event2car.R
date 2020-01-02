#' event2car: Calculates \code{CAR} (cumulative abnormal returns) for firm(s) and event(s)
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
#' If \code{market_model} is \emph{mrkt_adj_out} or \code{mrkt_adj_within}
#' and \code{regressor} has the length greater than one, the first element of
#' \code{regressor} will be applied for each security in \code{returns}.
#'
#' @param returns an object of \code{data.frame} or \code{zoo} containing rates of returns of securities.
#' @param regressor an object of the same class as \code{returns} containing regressors.
#'                  The argument can be omitted, if market model is \code{mean_adj}.
#' @param event_date an character object or an object of class \code{Date} containing one event date or multiple event dates in the format YYYY-MM-DD.
#' @param estimation_period an object of class \code{intenger} stating the number of days
#'  prior to the event over which the market model parameters are estimated. Default is 150 days.
#'  Note that the event period itself is not included in the event period to prevent the event from influencing the normal performance model parameter estimates.
#' @param car_lag an object of class \code{intenger} measuring the start of the event window. The default is 1 day prior to the event date.
#' @param car_lead an object of class \code{intenger} measuring the end of the event window. The default is 5 days after the event date.
#' @param method a character indicating the method used to calculate abnormal returns and cumulative abnormal returns. Choose among
#' \code{mean_adj}, \code{mrkt_adj_out}, and \code{mrkt_adj_within}.

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
#' @importFrom stats time window lm
#'
#' @keywords eventstudy stock finance
#'
#' @examples
#' data('tech_returns')
#' # prepare data
#' trumpelection <- as.Date("2016-11-08")
#' returns_firms <- tech_returns[,2:19]
#' return_indx <- tech_returns[,1]
#' # mean adjusted model
#' event2car(returns=returns_firms,regressor=return_indx,
#'           event_dates=trumpelection,market_model="mean_adj")
#' # market adjusted model (out-of sample estimation)
#' event2car(returns=returns_firms,regressor=return_indx,
#'           event_dates=trumpelection,market_model="mrkt_adj_out")
#' # market adjusted model (within sample estimation)
#' event2car(returns=returns_firms,regressor=return_indx,
#'           event_dates=trumpelection,market_model="mrkt_adj_within")

#' @export
event2car <- function(returns = NULL,regressor = NULL,event_date = NULL,
                      method = c("mean_adj","mrkt_adj_within","mrkt_adj_out"),
                      imputation = c("mean","none","pmm"),
                      car_lag = 1,car_lead = 5,estimation_period = 150){


  # Sanity Checks
  method <- match.arg(method)
  imputation <- match.arg(imputation)

  event_date <- as.Date(event_date)

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


  # Handling missing data
  ## mean imputation
  if (imputation == "mean") {
    if (any(colSums(is.na(returns))>0)) {
      missing <- colnames(returns)[colSums(is.na(returns))>0]
      warning(paste("Imputed data using mean returns of the following firm(s):",
                    paste(missing, collapse=' ')))
    }
    returns <- zoo::na.aggregate(returns,FUN=mean,na.rm=T)
  }
  ## no imputation: dropping firms with NAs
  if (imputation == "none") {
    if (any(colSums(is.na(returns))>0)) {
      missing <- colnames(returns)[colSums(is.na(returns))>0]
      warning(paste("Drop following firm(s) due to missing data:",
                    paste(missing, collapse=' ')))
    }
    returns <- returns[,colSums(is.na(returns))==0]
  }
  ## pmm: predictive mean matching
  if (imputation == "pmm") {
    if (library("mice",logical.return=TRUE)) {
      if (any(colSums(is.na(returns))>0)) {
        missing <- colnames(returns)[colSums(is.na(returns))>0]
        warning(paste("Imputed data using predictive mean matching of the following firm(s):",
                      paste(missing, collapse=' ')))
      }


     dates_returns <- stats::time(returns)
     returns <- mice::complete(mice::mice(returns,method="pmm",printFlag = FALSE))
     row.names(returns) <- dates_returns
     returns <- zoo::zoo(returns)
     zoo::index(returns) <- dates_returns
    } else {
      stop("'mice' package not installed. Choose another imputation method or run install.packages('mice')")
    }
  }

  # Estimate abnormal returns, confidence intervals,
  # and the cumulative abnormal return for each event and each firm
  temp <- lapply(event_date,function(event){
    ## Mean adjusted model
    if (method == "mean_adj") {
      ### data prep
      #### estimation period data
      y <- window(returns, start=(event-estimation_period-car_lag),end=(event-car_lag))
      #### event period data
      y2 <- window(returns, start=(event-car_lag),end=(event+car_lead))
      ### abnormal returns
      ar <- sapply(seq_len(ncol(y)),function(i){
        as.numeric(y2[,i]) - mean(as.numeric(y[,i]))})

      ci <- sapply(seq_len(ncol(ar)),function(i){
        se <- qnorm(0.975)*sd(ar[,i])/sqrt(nrow(ar))
        m <- mean(ar[,i])
        c(m-se, m+se)
      })
      ci <- t(ci)
      ### output
      out <- cbind(colMeans(ar),ci)
      colnames(out) <- c("ar_mean","ci_lower","ci_upper")

      out <- data.frame(out)
      out$car <- colSums(ar)
      out$firm <- names(y)
      row.names(out) <- NULL

      out <- out[c(5,1:4)]
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
      out <- cbind(coef(z)[3,],
                   confint(z)[rep(c(FALSE,FALSE,TRUE),ncol(y)),])
      colnames(out) <- c("ar_coef","ci_lower","ci_upper")

      out <- data.frame(out)
      out$car <- out$ar * sum(ifelse(x2==TRUE,1,0))
      out$firm <- rownames(out)
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
      y2 <- stats::window(returns, start=(event-car_lag),end=(event+car_lead))
      x12 <- stats::window(regressor, start=(event-car_lag),end=(event+car_lead))
      ### regression
      z <- stats::lm(y~x1)
      ### prediction or abnormal returns
      ar <- sapply(seq_along(x12),function(i){
        y2[i, ] - (coef(z)[1, ] + coef(z)[2, ] * x12[[i]])
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
      out$firm <- rownames(out)
      row.names(out) <- NULL

      out <- out[c(5,1:4)]
    }
    return(out)
  })
  names(temp) <- event_date
  return(temp)
}



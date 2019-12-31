

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
  if (min(time(returns))>=min(event_date)-estimation_period-car_lag | max(time(returns))<=max(event_date)-car_lead){
    stop("Length of returns is too short.
                     Either use a longer returns object or shorten the estimation- or event period.")
  }

  if (method %in% c("mrkt_adj_within","mrkt_adj_out")){

    if (NROW(returns) != NROW(regressor)) {
      stop("Length of returns is not equal to length of regressor")
    }
    if (NROW(regressor) < estimation_period+car_lag+car_lead) {
      stop("Length of regressor is too short.
                         Either use a longer returns object or shorten the estimation- or event period.")
    }
    if (min(time(regressor))>=min(event_date)-estimation_period-car_lag | max(time(regressor))<=max(event_date)-car_lead){
      stop("Length of regressor is too short.
                     Either use a longer regressor object or shorten the estimation- or event period.")
    }
    if (any(!time(regressor) %in% time(regressor))) {
      warning("regressor and returns have different time ranges.")
    }
  }


  # Handling missing data
  ## mean imputation
  if (imputation == "mean") {
    if (any(colSums(is.na(returns))>0)) {
      missing <- colnames(returns)[colSums(is.na(returns))>0]
      warning(paste("Imputed data using mean returns mean of the following firm(s):",
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


     dates_returns <- time(returns)
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
      x2 <- time(y) %in% seq(as.Date(event-car_lag), as.Date(event+car_lead), "days")
      ### regression inkl. event period
      z <- lm(y~x1+x2)
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
      y <- window(returns, start=(event-estimation_period-car_lag),end=(event-car_lag))
      x1 <- window(regressor, start=(event-estimation_period-car_lag),end=(event-car_lag))
      #### event period data
      y2 <- window(returns, start=(event-car_lag),end=(event+car_lead))
      x12 <- window(regressor, start=(event-car_lag),end=(event+car_lead))
      ### regression
      z <- lm(y~x1)
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


# test

event2car(returns = tech_returns[,2:19],regressor = tech_returns[,1],
          event_date = c("2019-09-01","2016-11-20","2018-11-19","2018-12-16"),
          method = "mean_adj",imputation = "pmm",
          car_lag = 1,car_lead = 5,estimation_period = 150)


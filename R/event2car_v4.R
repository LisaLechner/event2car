# datensatz aufbauen

event_date = as.Date("2016-11-20")
event_date = as.Date("2018-11-19")
event_date = as.Date("2018-12-16")
returns = tech_returns[,2:19]
regressor = tech_returns[,1]
car_lag = 1
car_lead = 5
estimation_period=150

event2car <- function(returns = NULL,regressor = NULL,event_date = NULL,
                      method = c("mean_adj","mrkt_adj_within","mrkt_adj_out"),
                      imputation = c("mean","none","pmm"),
                      car_lag = 1,car_lead = 5,estimation_period = 150){

  ########################################
  # sanity checks
  ########################################
  method <- match.arg(method)
  imputation <- match.arg(imputation)

  event_date <- as.Date(event_date)

  if (NROW(returns) < estimation_period+car_lag+car_lead) {
    stop("Length of returns is too short.
                     Either use a longer returns object or shorten the estimation or event period.")
  }

  if (method %in% c("mrkt_adj_within","mrkt_adj_out")){

    if (NROW(returns) != NROW(regressor)) {
      stop("Length of returns is not equal to length of regressor")
    }
    if (NROW(regressor) < estimation_period+car_lag+car_lead) {
      stop("Length of regressor is too short.
                         Either use a longer returns object or shorten the estimation or event period.")
    }
    if (any(!time(regressor) %in% time(regressor))) {
      warning("regressor and returns have different time ranges.")
    }
  }

  #------------
  # handling missing data
  #------------

  if (imputation == "mean") {
    # fast mean imputation
    if (any(colSums(is.na(returns))>0)) {
      missing <- colnames(returns)[colSums(is.na(returns))>0]
      warning(paste("Imputed data using mean returns mean of the following firm(s):",
                    paste(missing, collapse=' ')))
    }
    returns <- zoo::na.aggregate(returns,FUN=mean,na.rm=T)
  }

  if (imputation == "none") {
    # drop firms with NAs
    if (any(colSums(is.na(returns))>0)) {
      missing <- colnames(returns)[colSums(is.na(returns))>0]
      warning(paste("Drop following firm(s) due to missing data:",
                    paste(missing, collapse=' ')))
    }
    returns <- returns[,colSums(is.na(returns))==0]
  }

  if (imputation == "pmm") {
    # mice
    if (any(colSums(is.na(returns))>0)) {
      missing <- colnames(returns)[colSums(is.na(returns))>0]
      warning(paste("Imputed data using predictive mean matching of the following firm(s):",
                    paste(missing, collapse=' ')))
    }
    returns <- mice::complete(mice::mice(returns,method="pmm",printFlag = FALSE))
    returns <- zoo::zoo(returns)
  }


  if (method == "mean_adj") {
    ########################################
    # mean adjusted
    ########################################

    #------------
    # data prep
    #------------
    # estimation period data
    y <- window(returns, start=(event_date-estimation_period-car_lag),end=(event_date-car_lag))
    # event period data
    y2 <- window(returns, start=(event_date-car_lag),end=(event_date+car_lead))

    #------------
    # abnormal returns
    #------------
    ar <- sapply(seq_len(ncol(y)),function(i){
      as.numeric(y2[,i]) - mean(as.numeric(y[,i]))})

    ci <- sapply(seq_len(ncol(ar)),function(i){
      se <- qnorm(0.975)*sd(ar[,i])/sqrt(nrow(ar))
      m <- mean(ar[,i])
      c(m-se, m+se)
    })
    ci <- t(ci)

    #------------
    # output
    #------------
    out <- cbind(colMeans(ar),ci)
    colnames(out) <- c("ar_mean","ci_lower","ci_upper")

    out <- data.frame(out)
    out$car <- colSums(ar)
    out$firm <- names(y)
    row.names(out) <- NULL

    out <- out[c(5,1:4)]
  }

  if (method == "mrkt_adj_within") {
    ########################################
    # market adjusted (within sample)
    ########################################

    #------------
    # data prep
    #------------
    y <- window(returns, start=(event_date-estimation_period-car_lag),end=(event_date+car_lead))
    x1 <- window(regressor, start=(event_date-estimation_period-car_lag),end=(event_date+car_lead))
    x2 <- time(y) %in% seq(as.Date(event_date-car_lag), as.Date(event_date+car_lead), "days")

    #------------
    # regression inkl. event period
    #------------
    z <- lm(y~x1+x2)

    #------------
    # output
    #------------
    out <- cbind(coef(z)[3,],
                 confint(z)[rep(c(FALSE,FALSE,TRUE),ncol(y)),])
    colnames(out) <- c("ar_coef","ci_lower","ci_upper")

    out <- data.frame(out)
    out$car <- out$ar * sum(ifelse(x2==TRUE,1,0))
    out$firm <- rownames(out)
    row.names(out) <- NULL

    out <- out[c(5,1:4)]
  }

  if (method == "mrkt_adj_out") {
    ########################################
    # market adjusted (out-of sample)
    ########################################

    #------------
    # data prep
    #------------
    # estimation period data
    y <- window(returns, start=(event_date-estimation_period-car_lag),end=(event_date-car_lag))
    x1 <- window(regressor, start=(event_date-estimation_period-car_lag),end=(event_date-car_lag))
    # event period data
    y2 <- window(returns, start=(event_date-car_lag),end=(event_date+car_lead))
    x12 <- window(regressor, start=(event_date-car_lag),end=(event_date+car_lead))

    #------------
    # regression
    #------------
    z <- lm(y~x1)

    #------------
    # prediction or abnormal returns
    #------------
    ar <- sapply(seq_along(x12),function(i){
      y2[i,] - (m[1,] + m[2,]*x12[[i]])})
    ci <- sapply(seq_len(nrow(ar)),function(i){
      se <- qnorm(0.975)*sd(ar[i,])/sqrt(ncol(ar))
      m <- mean(ar[i,])
      c(m-se, m+se)
    })
    ci <- t(ci)

    #------------
    # output
    #------------
    out <- cbind(rowMeans(ar),ci)
    colnames(out) <- c("ar_mean","ci_lower","ci_upper")

    out <- data.frame(out)
    out$car <- rowSums(ar)
    out$firm <- rownames(out)
    row.names(out) <- NULL

    out <- out[c(5,1:4)]
  }

  return(out)
}

# test

event2car(returns = tech_returns[,2:19],regressor = tech_returns[,1],event_date = "2018-12-01",
          method = "mrkt_adj_within",imputation = "mean",
          car_lag = 1,car_lead = 5,estimation_period = 150)


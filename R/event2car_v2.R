## two firms
set.seed(1)
y <- cbind(rnorm(140), rnorm(140), rnorm(140), rnorm(140), rnorm(140),
           rnorm(140), rnorm(140), rnorm(140), rnorm(140), rnorm(140),
           rnorm(140), rnorm(140), rnorm(140), rnorm(140), rnorm(140),
           rnorm(140), rnorm(140), rnorm(140), rnorm(140), rnorm(140),
           rnorm(140), rnorm(140), rnorm(140), rnorm(140), rnorm(140),
           rnorm(140), rnorm(140), rnorm(140), rnorm(140), rnorm(140),
           rnorm(140), rnorm(140), rnorm(140), rnorm(140), rnorm(140),
           rnorm(140), rnorm(140), rnorm(140), rnorm(140), rnorm(140),
           rnorm(140), rnorm(140), rnorm(140), rnorm(140), rnorm(140),
           rnorm(140), rnorm(140), rnorm(140), rnorm(140), rnorm(140),
           rnorm(140), rnorm(140), rnorm(140), rnorm(140), rnorm(140),
           rnorm(140), rnorm(140), rnorm(140), rnorm(140), rnorm(140),
           rnorm(140), rnorm(140), rnorm(140), rnorm(140), rnorm(140),
           rnorm(140), rnorm(140), rnorm(140), rnorm(140), rnorm(140),
           rnorm(140), rnorm(140), rnorm(140), rnorm(140), rnorm(140),
           rnorm(140), rnorm(140), rnorm(140), rnorm(140), rnorm(140),
           rnorm(140), rnorm(140), rnorm(140), rnorm(140), rnorm(140),
           rnorm(140), rnorm(140), rnorm(140), rnorm(140), rnorm(140),
           rnorm(140), rnorm(140), rnorm(140), rnorm(140), rnorm(140),
           rnorm(140), rnorm(140), rnorm(140), rnorm(140), rnorm(140))
## indicator: 0 = not considered, 1 = estimation, 2 = prediction
x <- rep(c(0, 1, 0, 2), c(10, 120, 5, 5))

## v1a: matrix operations with logical index vector (of length n)
system.time(sum(x == 2) * (colMeans(y[x == 2,]) - colMeans(y[x == 1, ])))

## v1b: integer i5ndex vectors of lengths n1 and n2
x1 <- which(x == 1)
x2 <- which(x == 2)
system.time(length(x2) * (colMeans(y[x2,]) - colMeans(y[x1, ]))) # this is the fastest

## v2: multivariate linear model
xx <- cbind(1, as.numeric(x == 2))
system.time(sum(x == 2) * lm.fit(xx[x > 0,], y[x > 0, ])$coefficients[2,])

c <- lm.fit(xx[x > 0,], y[x > 0, ])$coefficients[2,]

z <- lm.fit(xx[x > 0,], y[x > 0, ])

r <- z$residuals
f <- z$fitted.values
rdf <- z$df.residual
colMeans(f)

mss <- colSums((f - colMeans(f))^2)
rss <- colSums(r^2)
resvar <- rss/rdf
r.squared <- mss/(mss + rss)

# se
sem <- sqrt(colSums(f - y[x > 0,])^2 / rdf)
ssx <- sum(xx[x>0,2] - mean(xx[x>0,2])^2)
seb <- sem/sqrt(ssx)


#TODO: first implement colmeans



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

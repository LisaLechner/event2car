# datensatz aufbauen

event_date = as.Date("2016-11-20")
event_date = as.Date("2018-11-19")
returns = tech_returns[,2:19]
regressor = tech_returns[,1]
car_lag = 1
car_lead = 5
estimation_period=250

# data preperation
estimation_period <- window(returns,start=(event_date-estimation_period-car_lag),end=(event_date-car_lag))
event_period <- window(returns,start=(event_date-car_lag),end=(event_date+car_lead))
tradingdays <- colSums(!is.na(event_period))
if(any(tradingdays<length(time(event_period)))){
  missing <- names(which(tradingdays<length(time(event_period))))
  warning(paste("Generated abnormal returns with missing event period data for the following firm(s):",paste(missing, collapse=' ')))}



# mean adjusted
car <- tradingdays * (colMeans(event_period,na.rm = T) - colMeans(estimation_period,na.rm = T)) #why times tradingdays?


#----------------------------
# market adjusted (within sample)
#----------------------------

# data prep
y <- window(returns, start=(event_date-estimation_period-car_lag),end=(event_date+car_lead))
x1 <- window(regressor, start=(event_date-estimation_period-car_lag),end=(event_date+car_lead))
x2 <- time(y) %in% seq(as.Date(event_date-car_lag), as.Date(event_date+car_lead), "days")


#xx <- cbind(x0=1,x1=as.numeric(x1),x2=ifelse(x2==TRUE,1,0))
#z <- lm.fit(xx, y[,c(1,3)])

# fast mean imputation

y <- zoo::na.aggregate(y,FUN=mean,na.rm=T)

m <- colnames(y)[colSums(is.na(y))>0]
m_mean <- apply(as.data.frame(y[,m]),2,function(x) mean(x,na.rm=T))

for(i in seq_along(m)){y[,m[i]] <- ifelse(is.na(y[,m[i]]),m_mean[[i]],y[,m[i]])}
z <- lm(y~xx[,2]+xx[,3])

# mice imputation

# drop observations with missings

z <- lm.fit(xx, y[,c(1,2)])


coef(z)

z <- lm(y[,c(1,2)]~xx[,2]+xx[,3])

coef(z)

# impute

xx <- cbind(x1=as.numeric(x1),x2=ifelse(x2==TRUE,1,0))
dt <- cbind(xx,matrix(y,nrow=nrow(y)))
dt <- mice(matrix(y,nrow=nrow(y)))
dt <- mice(dt,method="pmm") #TODO: check-out how this imputation is done
dt <- complete(dt)
names(dt) <- c("x1","x2",names(y))

lm(as.matrix(dt[,3:ncol(dt)])~as.matrix(dt[,1:2]))



w <- ifelse(is.na(y),0,1)
y <- zoo::na.locf(y)
y <- zoo::na.locf(y,fromLast=TRUE)

z <- lm.fit(xx, y,w)


z <- lm.fit(as.matrix(xx[which(!is.na(y[,2])),]),y[!is.na(y[,2]),2])

xx <- cbind(x1=as.numeric(x1),x2=ifelse(x2==TRUE,1,0))
lm(y[,c(1,3)]~xx)
lm(y[]~xx)

apply(is.na(y),2,any)

z <- lm.fit(xx, y)

z <- lm(xx, y)


xx <- cbind(1, as.numeric(x == 2))
system.time(sum(x == 2) * lm.fit(xx[x > 0,], y[x > 0, ])$coefficients[2,])

c <- lm.fit(xx[x > 0,], y[x > 0, ])$coefficients[2,]



returns = as.matrix(returns)
col2[which(is.nan(col2))] = NA
col2[which(col2==Inf)] = NA

col2[which(is.nan(col2))] = NA
col2[which(col2==Inf)] = NA

lm.fit(as.matrix(regressor),as.matrix(returns[,1]),na.action=na.exclude)
return ~ interept + regressor

lm.fit(xx[x > 0,], y[x > 0, ])$coefficients[2,]

#
((event_date-250):event_date) %in% time(returns)

wModel <- function (name, ...) {
  o <- structure(c(list(name=name), list(...)), class=paste('wModel_', name, sep=''))
}

fitStoreDeptSales <- function (x, ...) {
  UseMethod("fitStoreDeptSales", x)
}

forecastStoreDeptSales <- function (x, ...) {
  UseMethod("forecastStoreDeptSales", x)
}


# fitStoreDeptSales <- function (timeSeries) {
#   
#   #lambda <- BoxCox.lambda(timeSeries)
#   
#   a.pqd <- c(2, 0, 1)
#   a.PQD <- c(1, 0, 0)
#   
#   if (USE_REGX) {
#     #ss <- Arima(timeSeries, order=a.pqd, seasonal=list(order=a.PQD, period=ONE_YEAR_WEEKS), xreg=train.xreg)  
#     #ss <- Arima(timeSeries, order=a.pqd, xreg=train.xreg)  
#     #ss <- bats(timeSeries, use.parallel=FALSE)
#     #ss <- nnetar(timeSeries, P=1)
#     #ss <- stlf(timeSeries, method="arima",xreg=train.xreg)
#     ss <- timeSeries
#     
#   } else {
#     ss <- Arima(timeSeries, order=a.pqd, seasonal=list(order=a.PQD, period=ONE_YEAR_WEEKS))  
#   }
# }
# 
# forecastStoreDeptSales <- function (fit) {
#   if (USE_REGX) {
#     #f <- forecast(fit, h = PREDICT_LENGTH, xreg=test.xreg)  
#     forecast<-stlf(fit, h=PREDICT_LENGTH, method="arima", s.window=3, xreg=train.xreg, newxreg=test.xreg)
#   } else {
#     f <- forecast(fit, h = PREDICT_LENGTH)  
#   }
# }
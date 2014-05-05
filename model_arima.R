
fitStoreDeptSales.wModel_arima <- function (x) {
  if (missing(x))
    stop("Need to specify model")
  
  if (!exists("timeSeries", x))
    stop("Need to specify timeSeries.")

  timeSeries <- x$timeSeries
  a.pqd <- c(0, 1, 1)
  a.PQD <- c(0, 1, 1)  
  x$fit <- Arima(timeSeries, order=a.pqd, seasonal=list(order=a.PQD, period=ONE_YEAR_WEEKS))
  # , xreg=train.xreg
  return(x)
}

forecastStoreDeptSales.wModel_arima <- function (x) {
  if (missing(x))
    stop("Need to specify model")
  if (!exists("fit", x))
    stop("Need to specify fit.")

  fit <- x$fit
  
  x$forecast <- forecast(fit, h = PREDICT_LENGTH)
  # xreg=test.xreg
  return(x)
}

testWModel <- function() {
  m1 <- wModel('arima', timeSeries=getStoreDeptTrainTS(1,1))
  print(class(m1))
  m1 <- fitStoreDeptSales(m1)
  print(class(m1))
  print(m1)
}
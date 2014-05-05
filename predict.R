library(doMC, quiet=T)
registerDoMC(8)

getStoreDeptTrainTS <- function (store, dept, dateFrom=TRAIN.FROM, dateTo=TRAIN.TO) {
  t <- train
  df <- t[t$Store==store & t$Dept==dept & t$Date >= dateFrom & t$Date <= dateTo,c('Weekly_Sales', 'IsHoliday')]
  tt <- ts(df[,'Weekly_Sales'], frequency=ONE_YEAR_WEEKS, start=c(0, getWeek('2010-02-05')))
}

getStoreDeptTrainDF <- function (store, dept, dateFrom=TRAIN.FROM, dateTo=TRAIN.TO) {
  t <- train
  df <- t[t$Store==store & t$Dept==dept & t$Date >= dateFrom & t$Date <= dateTo,c('Store', 'Dept', 'Date', 'Weekly_Sales', 'IsHoliday')]
}


predictStoreDeptSales <- function (store, dept, modelName='arima', dateFrom=TRAIN.FROM, dateTo=TRAIN.TO) {
  tt <- getStoreDeptTrainTS(store, dept, dateFrom=dateFrom, dateTo=dateTo)
  
  m <- wModel(modelName, timeSeries = tt)
  
  m <- fitStoreDeptSales(m)
  m <- forecastStoreDeptSales(m)
  f <- m$forecast
  
  offset <- dateFrom - weeks(4)
  f.df <- data.frame(Store=store, Dept=dept, (lapply(round(time(f$mean)*ONE_YEAR_WEEKS), function(x) { offset  + weeks(x) })), as.numeric(f$mean))
  names(f.df) <- c( 'Store', 'Dept', 'Date', 'Weekly_Sales')
  
  return(f.df)
}

predictAllSales <- function () {
  res <- dlply(unique(test[,c('Store', 'Dept')]), .(Store, Dept), function (row) {
    tryCatch({
      print(paste(row$Store, row$Dept))
      p <- predictStoreDeptSales(row$Store, row$Dept)
    }, error = function(e) {
      print(paste(e, row$Store, row$Dept))
      return(data.frame(Store=row$Store, Dept=row$Dept, Date=test[test$Store==row$Store & test$Dept==row$Dept,]$Date, Weekly_Sales=0))
    })
  })
  res <- do.call("rbind", res)
  res <- join(test, res, type="left")
  res <- res[order(res$Store, res$Dept, res$Date),]
  res$Id <- paste(res$Store, res$Dept, res$Date, sep='_')
  
  predicts <<- res
  
  return(res)
}

predictAllSalesMC <- function () {
  
  storeDepts <- as.list(unique(test[,c('Store', 'Dept')]))
  
  res <- foreach(i=1:length(storeDepts$Store),.combine=rbind) %dopar% {
    
    storeDept.Store <- storeDepts$Store[i]
    storeDept.Dept <- storeDepts$Dept[i]
    
    tryCatch({
      print(paste(storeDept.Store, storeDept.Dept))
      p <- predictStoreDeptSales(storeDept.Store, storeDept.Dept)
    }, error = function(e) {
      print(paste(e, storeDept.Store, storeDept.Dept))
      return(data.frame(Store=storeDept.Store, Dept=storeDept.Dept, Date=test[test$Store==storeDept.Store & test$Dept==storeDept.Dept,]$Date, Weekly_Sales=0))
    })
  }
  
  #res <- do.call("rbind", res)
  res <- join(test, res, type="left")
  res <- res[order(res$Store, res$Dept, res$Date),]
  res$Id <- paste(res$Store, res$Dept, res$Date, sep='_')
  
  predicts <<- res
  
  return(res)
  
}

testPredict <- function (store, dept) {
  p1 <- predictStoreDeptSales(store, dept, dateTo=TRAIN.MID)
  p1 <- p1[p1$Date <= TRAIN.TO,]

  t1 <- getStoreDeptTrainDF(store, dept, dateFrom=min(p1$Date), dateTo=max(p1$Date))
  
  test.res <- join(p1, t1, by=c('Store', 'Dept', 'Date'))
  colnames(test.res)[4] <- 'Weekly_Sales.pred'
  
  print(ggplot(data=test.res) + geom_bar(aes(x=Date, y=Weekly_Sales.pred-Weekly_Sales), stat ="identity"))
  return(test.res)
}

savePredicts <- function() {
  write.table(predicts[,c('Id', 'Weekly_Sales')],file=paste("./data/submit_", format(Sys.time(), "%Y%m%d_%X"), ".csv", sep=''), row.names=FALSE, quote = FALSE, sep=",")
}

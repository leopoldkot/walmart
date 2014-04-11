checkIsHolidayConsistency <- function () {
  t1 <- unique(subset(features, IsHoliday == TRUE)[,c('Date','IsHoliday')])
  t2 <- unique(subset(train, IsHoliday == TRUE)[,c('Date','IsHoliday')])
  t3 <- unique(subset(test, IsHoliday == TRUE)[,c('Date','IsHoliday')])
  t <- unique(rbind(t1, t2, t3))
  
  t1 <- lapply(t$Date, function (d) {nrow(subset(features, Date == d & IsHoliday == FALSE))})
  t2 <- lapply(t$Date, function (d) {nrow(subset(train, Date == d & IsHoliday == FALSE))})
  t3 <- lapply(t$Date, function (d) {nrow(subset(test, Date == d & IsHoliday == FALSE))})
  
  t.m <- as.matrix(rbind(t1, t2, t3))
  
  idxs <- which(!t.m == 0)
  if (length(idxs) == 0) {
    print('IsHoliday is consistent');  
  } else {
    print('IsHoliday is NOT consistent');  
  }
  
  return(t)
}

calcSalesStats <- function() {
  
  salesStats <- ddply(train[train$Dept != 0,], .(Store, Dept), summarize, sales.total=sum(Weekly_Sales), sales.mean=mean(Weekly_Sales))
  
  res <- dlply(unique(train[,c('Store', 'Dept')]), .(Store, Dept), function (row) {
    tryCatch({
      s <- stl(getStoreDeptTrainTS(row$Store, row$Dept), s.window='periodic')$time.series
      t1 <- s[,"trend"]
      fit <- lm(t1~time(t1))
      r2 <- summary(fit)$adj.r.squared
      
      return(data.frame(Store=row$Store, Dept=row$Dept, trend.r2=r2))
    }, error = function(e) {
      print(paste(e, row$Store, row$Dept))
      return(data.frame(Store=row$Store, Dept=row$Dept, trend.r2=0))
    })
  })
  res <- do.call("rbind", res)
  res <- res[order(res$Store, res$Dept),]
  
  salesStats <- join(salesStats,res, by=c('Store', 'Dept'))
  salesStats <<- salesStats
}

getEasterSeries <- function(easterYear, offsetWeek) {
  fromWeek <- 12
  toWeek <- 17
  
  t1 <- train[ train$Dept != 0
               & !is.na(train$WeekOfTheYear) 
               & year(train$Date)==easterYear 
               & (fromWeek + offsetWeek) <= train$WeekOfTheYear & toWeek + offsetWeek >= train$WeekOfTheYear, 
               c('Store', 'Dept', 'WeekOfTheYear', 'Weekly_Sales')]
  t1$WeekOfTheYear <- t1$WeekOfTheYear - offsetWeek
  t1$year <- easterYear
  
  return(t1)
}

diffEasters <- function(year, WeekOfTheYear, Weekly_Sales) {
  dfE <- data.frame(year, WeekOfTheYear, Weekly_Sales)
  eCnt <- ddply(dfE, .(year), summarise, cnt=length(year))
  
  if (nrow(eCnt) == 3) {
    #print(eCnt)
    t1 <- cast(dfE, WeekOfTheYear~year, value='Weekly_Sales')
    v <- (abs(t1['2011'] - t1['2010']) + abs(t1['2012'] - t1['2011'])) 
    
    # print(head(t1, 5))
    
    return(sqrt(sum(v ^ 2)))
  } else {
    return(0)    
  }
  
}

calcEaster <- function(offset=c(0, 0, 0)) {
  yearOffsets0 <- list(c(2010, offset[1]), c(2011, offset[2]), c(2012, offset[3]))
  
  res <- list()
  for (yearOffset in yearOffsets0) {
    print(paste(yearOffset[1], yearOffset[2]))
    t <- getEasterSeries(yearOffset[1], yearOffset[2])
    
    #
    res[[length(res)+1]] <- t
  }
  #print(head(res))
  res <- do.call("rbind", res)
  
  tres <- ddply(res, c('Store', 'Dept'), summarise, diffEasters=diffEasters(year, WeekOfTheYear, Weekly_Sales))
  
  return(tres)
}

calcEasters <- function() {
  t0 <- calcEaster()
  t1 <- calcEaster(c(2,-1,1))
  t2 <- rbind(cbind(t0, offset="offset_no"),cbind(t1, offset="offset_yes"))
  
  res <- dcast(t2, Store + Dept ~ offset, value.var='diffEasters')
  
  return(res)
}
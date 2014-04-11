calcXRegs <- function () {
  train.xreg <<- getXReg(train)
  test.xreg <<- getXReg(test)
}

getXReg <- function(data) {
  hdf <- unique(subset(data, !is.na(IsHoliday))[,c('Date','IsHoliday')])
  
  hmy <- list(
    list(year=2010, weeks=c(6, 13, 36, 47, 52)),
    list(year=2011, weeks=c(6, 16, 36, 47, 52)),
    list(year=2012, weeks=c(6, 14, 36, 47, 52)),
    list(year=2013, weeks=c(5, 12, 35, 46, 51))
  )
  
  res <- c()
  
  for (hm in hmy) {
    pres <- c()
    i <- 0
    for (hw in hm$weeks) {
      i <- i + 1
      print(paste(hm$year, hw))
      wdf <- subset(hdf, year(Date) == hm$year)
      holidays <<- model.matrix(~ ifelse(getWeek(Date) == hw & year(Date) == hm$year, 1, 0), wdf)[,2:2]
      
      if (length(holidays) > 0) {
        xreg <- as.vector(filter(holidays, c(1/3), sides = 2, circular = TRUE))
        xregBefore <- as.vector(filter(shiftVec(holidays, -2), c(1/9, 2/9), sides = 1, circular = TRUE))
        xregAfter <- as.vector(filter(shiftVec(holidays, 1), c(2/9, 1/9 ), sides = 1, circular = TRUE))
        
        wRegs <- cbind(xregBefore, xreg, xregAfter)
        colnames(wRegs) <- c(paste("H.", i, ".B", sep=''), paste("H.", i, sep=''), paste("H.", i, ".A", sep=''))
        
        pres <- cbind(pres, wRegs)
      }
    }
    res <- rbind(res, pres)
  }
  
  #diagRes <-as.matrix(c(seq(from=as.integer(min(hdf$Date)), to=as.integer(max(hdf$Date)), by=7)))
  #diagRes <- apply(diagRes, 2, cumsum)
  #res <- cbind(res,diagRes)
  
  res <- addNoise(res)
  #  res <- apply(res, 2, cumsum)
  return (res)
}

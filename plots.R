geomHolidays <- function () {
  
  m <- c(unique(subset(features, IsHoliday == TRUE)[,c('Date')]),
         unique(subset(test, IsHoliday == TRUE)[,c('Date')])
  )
  
  m <- data.frame(HolidayDate=m, color='blue')
  t1 <- data.frame(HolidayDate=(as.Date(c('2010-04-04', '2011-04-24', '2012-04-08', '2013-03-31')) - 2), 
                   color='magenta')
  
  m <- rbind(m, t1)
  m$HolidayDate <- as.numeric(m$HolidayDate)
  localenv <- environment()
  geom_vline(aes(xintercept=HolidayDate, colour = color), data=m, environment = localenv)
}

geomMilestones <- function () {
  dates <- c('2010-02-05', '2012-10-26', '2012-11-02', '2013-07-26')
  dates <- as.numeric(as.Date(dates))
  #dates <- c(dates, as.Date('2010-02-05') + weeks(52*2))
  geom_vline(xintercept=dates, colour = "green")
}

plotMilestones <- function () {
  (ggplot() 
   + xlim(as.Date('2010-02-05'), as.Date('2013-07-26')) 
   + ylim(0, 1) 
   + geomMilestones()
   + geomHolidays()
   + scale_color_identity()
  )
}

plotFuel <- function (store) {
  p1 <- (
    ggplot(features[features$Store==store,], aes(x=Date,y=Fuel_Price/CPI_s))
    + geom_point()
    + geom_smooth(method='loess')
  )
  return (p1)
}

plotUnemp <- function (store) {
  p1 <- (
    ggplot(features[features$Store==store,], aes(x=Date,y=Unemployment))
    + geom_point()
    + geom_smooth(method='loess')
  )
  return (p1)
}

plotSales <- function (store, dept) {
  t <- train[train$Store==store & train$Dept==dept,]
  p <- (
    ggplot(t, aes(x=Date,y=Weekly_Sales))
    + geom_point()
    + geom_smooth()
    + geomHolidays()
    #+ geom_vline(xintercept=as.numeric(train[train$IsHoliday == TRUE,]$Date), colour = "blue")
  )
  
  print(multiplot(p,plotFuel(store)))
  #qplot(t$Date, t$Weekly_Sales)
}

plotStoreSales <- function (store, deptFrom=1, deptTo=10) {
  fields <- c('Store', 'Dept', 'Date', 'Weekly_Sales')
  
  cmp <- expression(Store == store & Dept >= deptFrom & Dept <= deptTo)
  t <- subset(train, eval(cmp))[,fields]
  if (exists('predicts')) {
    t <- rbind(t, subset(predicts, eval(cmp))[,fields])
  }
  
  p <- ( ggplot() 
         + geom_bar(data=t, aes(x=Date,y=(Weekly_Sales), fill=(Dept),order=Dept), stat="identity", position = "stack")
         + facet_grid(Dept ~ ., scale="free_y")
         + geomHolidays()
         + geomMilestones()
         + guides(fill=FALSE)
  )
  
  multiplot(plotStoreMarkdowns(store) + guides(fill=FALSE) + theme(legend.margin=unit(-0.6,"cm")) , p)
  
  #qplot(t$Date, t$Weekly_Sales)
}

plotDeptSales <- function (dept) {
  fields <- c('Store', 'Dept', 'Date', 'Weekly_Sales')
  
  cmp <- expression(Dept == dept & Store >= 0 & Store < 20)
  
  if (exists('predicts')) {
    t <- rbind(subset(train, eval(cmp))[,fields], subset(predicts, eval(cmp))[,fields])
  } else {
    t <- subset(train, eval(cmp))[,fields]
  }
  
  t$nCoef <- as.numeric(max(train$Date) - min(train$Date))/7
  
  p <- ( ggplot() 
         + geom_bar(data=t, aes(x=Date,y=(Weekly_Sales), fill=(Store)), stat="identity", position = "stack")
         + facet_grid(Store ~ ., scale="free_y")
         + geomHolidays()
         + geomMilestones()
  )
  multiplot(p)
}


plotStoreMarkdowns <- function (store) {
  cols <- c('Store', 'Date', 'IsHoliday')
  cols <- append(cols, sapply(seq(1, 5), function(i) {paste('MarkDown', i, sep="")}))
  t <- subset(features[cols], Store == store)
  m <- melt(t, id=c('Date', 'Store', 'IsHoliday'))
  #m <- na.omit(m)
  m[is.na(m$value),]$value <- 0
  #print(head(m))
  
  #print(paste(min(m$Date), max(m$Date)))
  
  p <- ( ggplot() 
         + geom_bar(data=m, aes(x=Date,y=(value), fill=(variable),order=variable), stat="identity", position = "stack")
         + facet_grid(variable ~ ., scale="free_y")
         + scale_x_date(breaks = "3 month", minor_breaks = "1 month")
         + geomMilestones()
         + geomHolidays()
  )
  #p <- p + scale_fill_brewer(palette="blues")
  #print(p)
}

plotPredict <- function (store, dept, train.dateTo=TRAIN.TO, modelName=DEFAULT_MODEL) {
  t <- train
  tt <- getStoreDeptTrainTS(store, dept, dateTo=train.dateTo)

  m <- wModel(modelName, timeSeries = tt)
  m <- fitStoreDeptSales(m)
  m <- forecastStoreDeptSales(m)
  f <- m$forecast
  plot(f)
}

plotAligns <- function(store, dept) {
  d <- subset(train, Store==store & Dept==dept)
  #
  dh <- d[d$IsHoliday == TRUE,]
  
  return(ggplot(data=d,aes(x=WeekOfTheYear,y=Weekly_Sales, fill=as.factor(year(Date))))
         + geom_bar(stat='identity', position="dodge", width=0.4)
         + scale_x_discrete(drop=FALSE) 
         + geom_vline(data=dh, aes(xintercept=WeekOfTheYear, color=as.factor(year(Date))))
  )
  
}

plotSTL <- function (store, dept) {
  t <- train
  tt <- getStoreDeptTrainTS(store, dept)
  
  s <- stl(tt, s.window=200)
  plot(s)
  
  t1 <- s$time.series[,"trend"]
  fit <- lm(t1~time(t1))
  sigma <- summary(fit)$adj.r.squared
  #print(sigma)
  
  return(t)
}

plotVariance <- function () {
  
  salesVarMat <- acast(salesStats, Store~Dept, value.var="trend.r2")
  salesVarMat[is.na(salesVarMat)] <- 0
  heatmap(salesVarMat,Rowv=NA, Colv=NA,col = heat.colors(1024), scale="column", margins=c(5,10),distfun=function (y) dist(y,method = "manhattan"))
  
  return(salesVarMat)
}


ggSTL <- function (x, xSTL) {
  data.frame(x,xSTL$time.series) -> x3 #creating a data.frame
  x3$Date <- as.Date('2010-02-05') + (index(x3) * 7) #create an id 
  colnames(x3)[1] <- "time-series"  
  melt(x3,id.vars="Date") -> x4   #melt the dataframe into long format  
  ggplot(x4,aes(Date,value,group=variable)) + geom_line(alpha=0.7) + facet_grid(variable ~., scales="free")  + xlab("Timesteps") + ylab("values")
}

saveAllPlots <- function () {
  
  for (i in 1:45) {
    pdf(file=paste("./plots/sales_store_", i, ".pdf", sep=''), width=12, height=50, title=paste('Store', i))
    plotStoreSales(i, 1, 99)
    dev.off()
  } 
}


plotAlignment <- function (store, dept) {
  t1 <- getStoreDeptTrainTS(store, dept)
  ts1 <- as.vector(t1)[1:52]
  ts2 <- as.vector(t1)[53:104]
  
  alignment<-dtw(ts1, ts2, keep=TRUE, window.type="sakoechiba", window.size=4);
  dtwPlot(alignment, type="three")
}
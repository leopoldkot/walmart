
# badIds <- list(c(5, 99), c(9, 99), c(10, 99), c(18,43), c(24, 43), c(25, 99), c(34, 39), c(36, 30), c(37, 29), c(42, 30))

# features  2010-02-05                            2012-10-26
# train     2010-02-05    2012-10-26
# test                                2012-11-02  2013-07-26
ONE_YEAR_WEEKS=52
PREDICT_LENGTH=39
EASTER <- c(13, 16, 14, 12)

TRAIN.FROM <- as.Date('2010-02-05')
TRAIN.FROM.NUM <- as.numeric(TRAIN.FROM)
TRAIN.TO <- as.Date('2012-10-26')
TRAIN.TO.NUM <- as.numeric(TRAIN.TO)
TRAIN.MID <- as.Date('2010-02-05') + weeks(52*2)

TEST.FROM <- as.Date('2012-11-02')


loadData <- function () {
  stores <<- read.csv('./data/stores.csv')
  features <<- read.csv('./data/features.csv')
  train <<- read.csv('./data/train.csv')
  test <<- read.csv('./data/test.csv')
  
  test$Date <<- as.Date(test$Date)
  train$Date <<- as.Date(train$Date)
  features$Date <<- as.Date(features$Date)
  train$IsHoliday <<- as.logical(train$IsHoliday)
  stores$size_s <<- scale(stores$Size)
  train <<- rbind(train, getTotalStoreSales())
  
  # Easter
  # 2010 - 13 2010-04-04
  # 2011 - 16 2011-04-24
  # 2012 - 14 2012-04-08
  # 2013 - 12 2013-03-31
  
  train$WeekOfTheYear <<- (getWeek(train$Date))
  test$WeekOfTheYear <<- (getWeek(test$Date))
  
  fillTrainNA()
  boostCPI()
  features <<- ddply(features, .(Date), transform, CPI_ss=mean(CPI_s))
  
  ws.stats <- ddply(train, ~Store, summarise, ws.sum = sum(Weekly_Sales), ws.mean = sum(Weekly_Sales)/length(Weekly_Sales != 0))
  
  stores <<- merge(stores, ws.stats, all.x = TRUE)
  
  features <<- merge(features, stores, all.x = TRUE)
  features <<- merge(features, subset(train, Dept == 0)[,c('Store', 'Date', 'Weekly_Sales')], all.x = TRUE)
  features <<- features[order(features$Store,features$Date),]
  
  train <<- merge(train, features[, c('Store', 'Date', 'CPI_s')], all.x=TRUE)
  train$ws_s <<- train$Weekly_Sales / train$CPI_s
}

fillTrainNA <- function () {
  t <- train
  t$dt <- as.numeric(t$Date)
  
  t.s <- merge(unique(train[,c('Store', 'Dept')]), data.frame(dt=seq(TRAIN.FROM.NUM, TRAIN.TO.NUM, by=7)), by=NULL)
  t.s <- join(t, t.s, by=c('Store', 'Dept', 'dt'), type="right")
  
  t.s$Date = as.Date(t.s$dt)
  t.s <- subset(t.s, select = -dt)
  
  t.s$Weekly_Sales[is.na(t.s$Weekly_Sales)] <- 0
  
  train <<- t.s
}

getTotalStoreSales <- function () {
  m <- melt(train, id.vars=c('Store', 'Date', 'IsHoliday'), measure.vars=c('Weekly_Sales'))
  m$Date <- as.Date(m$Date)
  #m <- subset(m, Date <= as.Date('2010-03-05'))
  m <- dcast(m, Store + Date + IsHoliday ~ ., sum, value.var = 'value')
  colnames(m)[4] <- 'Weekly_Sales'
  m$Dept <- seq(0, 0, nrow(m))
  m <- m[, c(1,5,2,4,3)]
  return(m)
}

boostCPI <- function () {
  models <<- getCPIModels()
  fits <<- getCPIFits(models, features)
  features <<- merge(features, do.call("rbind", fits), by=c('Store', 'Date'), all.x = TRUE)
  features$CPI_s <<- ifelse(is.na(features$CPI_s), features$CPI_sp, features$CPI_s)
  
  fits <<- getCPIFits(models, test)
  test <<- join(test, do.call("rbind", fits), by=c('Store', 'Date'), type="left", match="first")
}

getCPIModels <- function () {
  features <<- ddply(features, .(Store), transform, CPI_s=zScore(CPI))
  models <- dlply(features, 'Store', function(df) { list(df$Store[1], lm(CPI_s ~ Date, data = df)) })
}

getCPIFits <- function (models, df) {
  fits <- lapply(models,
                 function (m) {
                   dateStoreCPI <- df[df$Store == m[[1]],] # is.na(df$CPI_s)
                   Date <- dateStoreCPI$Date
                   CPI_sp <- predict(m[[2]], dateStoreCPI)
                   res <- data.frame(Store=rep(m[[1]], length(Date)), Date=Date, CPI_sp=CPI_sp)
                   return(res)
                 }
  )
}

storeDept <- function (data, store, dept) {
  return (subset(data, Store==store & Dept==dept))
}
